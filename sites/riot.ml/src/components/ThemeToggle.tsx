import { Button } from "@cloudflare/kumo";
import { MoonIcon, SunIcon } from "@phosphor-icons/react";

type Mode = "light" | "dark";

const storageKey = "riot-theme";

function currentMode(): Mode {
  return document.documentElement.dataset.mode === "dark" ? "dark" : "light";
}

function applyMode(mode: Mode) {
  document.documentElement.dataset.mode = mode;
  try {
    window.localStorage.setItem(storageKey, mode);
  } catch {
    // Storage can be unavailable (private mode); the choice still applies.
  }
}

/**
 * Light/dark switch. Both icons render on the server and CSS shows the one
 * matching `data-mode`, so there is no flash before hydration.
 */
export function ThemeToggle() {
  return (
    <Button
      variant="ghost"
      shape="square"
      aria-label="Toggle color theme"
      title="Toggle color theme"
      onClick={() => applyMode(currentMode() === "dark" ? "light" : "dark")}
    >
      <SunIcon size={16} className="hidden [[data-mode=dark]_&]:block" />
      <MoonIcon size={16} className="block [[data-mode=dark]_&]:hidden" />
    </Button>
  );
}
