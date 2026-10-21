import { ClipboardText } from "@cloudflare/kumo";

export const installCommand = "curl -sSL https://get.riot.ml | sh";

export function InstallCommand({ size = "lg" }: { size?: "sm" | "base" | "lg" }) {
  return (
    <ClipboardText
      size={size}
      text={installCommand}
      className="w-full max-w-md font-mono"
      labels={{ copyAction: "Copy install command" }}
    />
  );
}
