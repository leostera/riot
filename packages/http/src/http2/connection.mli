open Std

(** HTTP/2 Connection Management (RFC 9113)

    This module manages HTTP/2 connections, handling:
    - Connection preface and settings negotiation
    - Stream multiplexing and lifecycle
    - Flow control (connection and stream level)
    - Priority handling
    - Error handling and connection shutdown
*)

(** {1 Types} *)

(** Connection state *)
type state =
  | Idle  (** Connection created but not started *)
  | Active  (** Connection active and can send/receive *)
  | GoingAway  (** Sent GOAWAY, finishing existing streams *)
  | Closed  (** Connection closed *)

(** Stream state per RFC 9113 Section 5.1 *)
type stream_state =
  | StreamIdle
  | StreamOpen
  | StreamReservedLocal
  | StreamReservedRemote
  | StreamHalfClosedLocal
  | StreamHalfClosedRemote
  | StreamClosed

(** Stream information *)
type stream = {
  id : int;
  state : stream_state Cell.t;
  window_size : int Cell.t;  (** Flow control window *)
  headers : Hpack.header list Cell.t;
  data_chunks : bytes list Cell.t;
}

(** Connection settings per RFC 9113 Section 6.5.2 *)
type settings = {
  header_table_size : int Cell.t;  (** HPACK dynamic table size *)
  enable_push : bool Cell.t;  (** Server push enabled *)
  max_concurrent_streams : int option Cell.t;  (** Max concurrent streams *)
  initial_window_size : int Cell.t;  (** Initial flow control window *)
  max_frame_size : int Cell.t;  (** Maximum frame size *)
  max_header_list_size : int option Cell.t;  (** Maximum header list size *)
}

(** Connection configuration *)
type config = {
  max_frame_size : int;  (** Maximum frame size we'll accept *)
  initial_window_size : int;  (** Initial window size for flow control *)
  max_concurrent_streams : int;  (** Maximum concurrent streams *)
  enable_push : bool;  (** Whether to enable server push *)
}

(** Connection handle *)
type t

(** Connection role *)
type role = Client | Server

(** {1 Connection Lifecycle} *)

(** Create a new HTTP/2 connection.

    @param role Whether this is a client or server connection
    @param config Connection configuration (optional, uses defaults)
    @return A new connection handle
*)
val create : role:role -> ?config:config -> unit -> t

(** Send the connection preface.

    For clients: Sends the connection preface string and SETTINGS frame.
    For servers: Sends just the SETTINGS frame.

    Per RFC 9113 Section 3.4:
    - Client preface: "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" + SETTINGS
    - Server preface: SETTINGS frame

    @param conn The connection
    @return Bytes to send over the wire
*)
val send_preface : t -> bytes

(** Process received data from the wire.

    Parses HTTP/2 frames and updates connection state accordingly.

    @param conn The connection
    @param data The received bytes
    @return Result with list of events or error
*)
val process_data : t -> bytes -> (event list, string) Result.t

(** {1 Stream Management} *)

(** Create a new stream.

    @param conn The connection
    @return Result with new stream ID or error
*)
val create_stream : t -> (int, string) Result.t

(** Send headers on a stream.

    @param conn The connection
    @param stream_id The stream ID
    @param headers The headers to send
    @param end_stream Whether this ends the stream (no data will follow)
    @return Result with bytes to send or error
*)
val send_headers :
  t -> stream_id:int -> headers:Hpack.header list -> end_stream:bool -> (bytes, string) Result.t

(** Send data on a stream.

    @param conn The connection
    @param stream_id The stream ID
    @param data The data to send
    @param end_stream Whether this is the last data frame
    @return Result with bytes to send or error
*)
val send_data :
  t -> stream_id:int -> data:bytes -> end_stream:bool -> (bytes, string) Result.t

(** Close a stream with error code.

    Sends RST_STREAM frame.

    @param conn The connection
    @param stream_id The stream ID
    @param error_code The error code
    @return Bytes to send
*)
val reset_stream : t -> stream_id:int -> error_code:Frame.error_code -> bytes

(** {1 Flow Control} *)

(** Send WINDOW_UPDATE frame for connection-level flow control.

    @param conn The connection
    @param increment Window size increment
    @return Bytes to send
*)
val send_window_update_connection : t -> increment:int -> bytes

(** Send WINDOW_UPDATE frame for stream-level flow control.

    @param conn The connection
    @param stream_id The stream ID
    @param increment Window size increment
    @return Bytes to send
*)
val send_window_update_stream : t -> stream_id:int -> increment:int -> bytes

(** Get current connection flow control window size. *)
val connection_window_size : t -> int

(** Get current stream flow control window size. *)
val stream_window_size : t -> stream_id:int -> int option

(** {1 Settings} *)

(** Update connection settings.

    Sends SETTINGS frame with new values.

    @param conn The connection
    @param settings The settings to update
    @return Bytes to send
*)
val update_settings : t -> Frame.setting list -> bytes

(** Send SETTINGS ACK frame.

    @param conn The connection
    @return Bytes to send
*)
val send_settings_ack : t -> bytes

(** Get current local settings. *)
val local_settings : t -> settings

(** Get current remote settings (from peer). *)
val remote_settings : t -> settings

(** {1 Connection Control} *)

(** Send PING frame.

    @param conn The connection
    @param data 8-byte opaque data
    @return Bytes to send
*)
val send_ping : t -> data:string -> bytes

(** Send PING ACK frame.

    @param conn The connection
    @param data 8-byte opaque data from received PING
    @return Bytes to send
*)
val send_ping_ack : t -> data:string -> bytes

(** Send GOAWAY frame to gracefully close the connection.

    @param conn The connection
    @param last_stream_id Last stream ID processed
    @param error_code Error code
    @param debug_data Optional debug data
    @return Bytes to send
*)
val send_goaway :
  t ->
  last_stream_id:int ->
  error_code:Frame.error_code ->
  ?debug_data:string ->
  unit ->
  bytes

(** Get connection state. *)
val state : t -> state

(** Close the connection immediately. *)
val close : t -> unit

(** {1 Events} *)

(** Events generated by processing received data *)
and event =
  | HeadersReceived of { stream_id : int; headers : Hpack.header list; end_stream : bool }
  | DataReceived of { stream_id : int; data : bytes; end_stream : bool }
  | SettingsReceived of Frame.setting list
  | SettingsAckReceived
  | PingReceived of { data : string }
  | PingAckReceived of { data : string }
  | WindowUpdateReceived of { stream_id : int; increment : int }
  | GoawayReceived of {
      last_stream_id : int;
      error_code : Frame.error_code;
      debug_data : string;
    }
  | RstStreamReceived of { stream_id : int; error_code : Frame.error_code }
  | PriorityReceived of {
      stream_id : int;
      dependency : int;
      weight : int;
      exclusive : bool;
    }

(** {1 Utilities} *)

(** Default connection configuration *)
val default_config : config

(** Check if stream ID is valid for the given role.

    Per RFC 9113: Client-initiated streams are odd, server-initiated are even.

    @param role The connection role
    @param stream_id The stream ID to check
*)
val is_valid_stream_id : role:role -> int -> bool
