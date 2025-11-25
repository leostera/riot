# Reentrant Parsers with IO.Reader

**Date**: 2025-11-25
**Status**: Implemented

## Overview

All HTTP/2 and gRPC parsers have been refactored to use `Std.IO.Reader` and be fully reentrant. This enables:

- **Streaming I/O**: Parse data as it arrives without buffering entire frames/messages
- **Non-blocking**: Parsers return `Need_more` when data is incomplete
- **State preservation**: Parsers maintain state between calls
- **Memory efficiency**: No upfront allocation of large buffers

## Architecture

### Old Approach (❌ Don't Use)
```ocaml
(* Required complete data upfront *)
val parse_frame : string -> (Frame.t, string) Result.t

(* Problems:
   - Must buffer entire frame before parsing
   - Blocking: waits for complete data
   - Memory intensive for large frames
   - Not composable with streaming I/O
*)
```

### New Approach (✅ Use This)
```ocaml
(* Reentrant with IO.Reader *)
type state
val create : unit -> state
val parse : state -> IO.Reader.t -> parse_result

type parse_result =
  | Frame of Frame.t      (* Complete *)
  | Need_more             (* Call again with more data *)
  | Error of string       (* Parse error *)

(* Benefits:
   - Incremental parsing
   - Non-blocking
   - Minimal memory overhead
   - State machine tracks progress
*)
```

## Parsers

### 1. HTTP/2 Frame Parser

**Module**: `Http.Http2.Parser_reader`

**State Machine**:
```
ReadingFrameHeader (9 bytes)
         │
         ├─ [complete] → ReadingFramePayload (N bytes)
         │                      │
         │                      └─ [complete] → Frame result
         │
         └─ [incomplete] → Need_more (resume later)
```

**Usage**:
```ocaml
let parser = Http.Http2.Parser_reader.create () in
let reader = IO.Reader.create tcp_stream in

let rec read_frames () =
  match Http.Http2.Parser_reader.parse parser reader with
  | Frame frame ->
      handle_frame frame;
      read_frames ()  (* Parse next frame *)
  | Need_more ->
      yield ();  (* Wait for more data *)
      read_frames ()
  | Error e ->
      handle_error e
```

**State Tracking**:
- Buffers only the current incomplete frame
- Tracks bytes read vs bytes needed
- Automatically transitions between header/payload phases
- Resets state after each complete frame

### 2. HPACK Decoder

**Module**: `Http.Http2.Hpack_reader`

**State Machine**:
```
WaitingForHeader
         │
         ├─ Indexed → lookup → add to headers → WaitingForHeader
         │
         ├─ LiteralWithIndexing
         │         │
         │         ├─ ReadingIndexedName → ReadingLiteralValue
         │         └─ ReadingLiteralName → ReadingLiteralValue
         │                                         │
         │                                         └─ add to table → WaitingForHeader
         │
         └─ [end of block] → Headers result
```

**Usage**:
```ocaml
let decoder = Http.Http2.Hpack_reader.create () in
let reader = IO.Reader.create stream in

match Http.Http2.Hpack_reader.decode decoder reader with
| Headers headers ->
    (* Got complete header block *)
    process_headers headers
| Need_more ->
    (* Need CONTINUATION frame or more data *)
    wait_for_data ()
| Error e ->
    handle_error e
```

**Features**:
- Handles multi-frame header blocks (HEADERS + CONTINUATION)
- Manages dynamic table automatically
- Supports all HPACK encoding types
- Validates header names and sizes

### 3. gRPC Message Parser

**Module**: `Grpc.Message_reader`

**State Machine**:
```
ReadingHeader (5 bytes)
         │
         ├─ [complete] → ReadingPayload (N bytes)
         │                      │
         │                      └─ [complete] → Message result
         │
         └─ [incomplete] → Need_more
```

**Usage**:
```ocaml
let parser = Grpc.Message_reader.create () in
let reader = IO.Reader.create stream in

let rec read_messages () =
  match Grpc.Message_reader.parse parser reader with
  | Message msg ->
      (* Got complete gRPC message *)
      let decoded = Protobuf.WireFormat.decode msg.payload in
      handle_message decoded;
      read_messages ()
  | Need_more ->
      yield ();
      read_messages ()
  | Error e ->
      handle_error e
```

**Features**:
- Validates message size before allocation
- Handles compressed flag
- Zero-copy payload extraction
- Configurable max message size

## Benefits

### 1. Memory Efficiency

**Before** (bytes-based):
```ocaml
(* Must buffer entire 16MB frame *)
let data = Buffer.create (16 * 1024 * 1024) in
read_fully stream data;
parse_frame (Buffer.contents data)
```

**After** (IO.Reader-based):
```ocaml
(* Buffers only what's needed incrementally *)
let parser = Parser_reader.create () in
match Parser_reader.parse parser reader with
| Frame _ -> (* Only buffered up to frame boundary *)
```

### 2. Non-Blocking I/O

**Before**:
```ocaml
(* Blocks waiting for complete frame *)
let frame = parse_frame (read_until_complete stream)
```

**After**:
```ocaml
(* Returns immediately if data not available *)
match Parser_reader.parse parser reader with
| Need_more -> (* Continue other work *)
| Frame _ -> (* Process frame *)
```

### 3. Composability

**Streaming pipeline**:
```ocaml
(* Chain parsers naturally *)
TcpStream → IO.Reader → Parser_reader → Frame
                            ↓
                      Hpack_reader → Headers
                            ↓
                      Message_reader → gRPC Message
```

### 4. Error Recovery

**Reentrant state allows retry**:
```ocaml
match Parser_reader.parse parser reader with
| Need_more ->
    (* Network timeout *)
    reconnect ();
    (* Resume from exact same position *)
    Parser_reader.parse parser reader
| Frame f -> handle_frame f
```

## Implementation Pattern

All parsers follow this pattern:

```ocaml
type parse_phase =
  | Phase1 of { buffer : Buffer.t; bytes_read : int; ... }
  | Phase2 of { ... }

type state = {
  config : config;
  phase : parse_phase Cell.t;  (* Mutable state *)
}

let parse state reader =
  match Cell.get state.phase with
  | Phase1 { buffer; bytes_read } ->
      let actual_read = read_n_bytes reader buffer needed in
      if bytes_read + actual_read < needed then (
        (* Update state *)
        Cell.set state.phase (Phase1 { buffer; bytes_read = bytes_read + actual_read });
        Need_more
      ) else (
        (* Transition to next phase *)
        Cell.set state.phase Phase2 { ... };
        parse state reader  (* Tail-recursive continuation *)
      )
  | Phase2 ... ->
      (* Similar pattern *)
```

**Key principles**:
1. **State in Cell**: Use `Cell.t` for mutable state (CLAUDE.md compliant)
2. **Buffer incrementally**: Only buffer incomplete fragments
3. **Tail recursion**: Continue parsing if data available
4. **Return Need_more**: Don't block, let caller retry
5. **Reset on complete**: Clear state after successful parse

## Testing Reentrant Behavior

**Test with partial data**:
```ocaml
let test_partial_parse () =
  let parser = Parser_reader.create () in

  (* Simulate receiving 5 bytes of 9-byte header *)
  let reader1 = IO.Reader.of_bytes (Bytes.of_string "\x00\x00\x00\x01\x04") in
  match Parser_reader.parse parser reader1 with
  | Need_more -> (* Expected *)
  | _ -> failwith "Should need more"

  (* Send remaining 4 bytes + payload *)
  let reader2 = IO.Reader.of_bytes (Bytes.of_string "\x00\x00\x00\x00...") in
  match Parser_reader.parse parser reader2 with
  | Frame f -> (* Success! Parser resumed correctly *)
  | _ -> failwith "Should parse complete frame"
```

## Migration Guide

**Old code**:
```ocaml
let data = read_all_bytes stream in
match Http.Http2.Parser.parse_frame data with
| Done { value; remaining } -> handle_frame value
| Need_more -> failwith "shouldn't happen"
| Error e -> handle_error e
```

**New code**:
```ocaml
let parser = Http.Http2.Parser_reader.create () in
let reader = IO.Reader.create stream in

let rec loop () =
  match Http.Http2.Parser_reader.parse parser reader with
  | Frame frame -> handle_frame frame; loop ()
  | Need_more -> yield (); loop ()
  | Error e -> handle_error e

loop ()
```

## Performance Characteristics

| Metric | Bytes-based | IO.Reader-based |
|--------|-------------|-----------------|
| Memory per frame | O(frame_size) | O(min(buffer, frame_size)) |
| Blocking | Yes | No |
| Partial data handling | Manual buffering required | Automatic |
| State management | Manual | Built-in |
| Composability | Low | High |

## Future Enhancements

1. **Zero-copy payloads**: Use `IO.Reader.peek` to avoid buffer copies
2. **Backpressure**: Signal when to stop reading
3. **Parallel parsing**: Parse multiple frames concurrently
4. **Metrics**: Track parse times and buffer sizes

## References

- [Std.IO.Reader documentation](../../std/src/IO/)
- [HTTP/2 RFC 9113](https://www.rfc-editor.org/rfc/rfc9113.html)
- [gRPC Protocol Specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
