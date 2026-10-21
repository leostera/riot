import { cn } from "@cloudflare/kumo";

interface CodeSampleProps {
  code: string;
  className?: string;
  label?: string;
}

/** A static, bordered code sample using Kumo surface tokens. */
export function CodeSample({ code, className, label }: CodeSampleProps) {
  return (
    <pre
      aria-label={label}
      className={cn(
        "overflow-x-auto rounded-lg bg-kumo-recessed px-4 py-3 font-mono text-[13px] leading-relaxed text-kumo-default ring ring-kumo-hairline",
        className,
      )}
    >
      <code>{code}</code>
    </pre>
  );
}
