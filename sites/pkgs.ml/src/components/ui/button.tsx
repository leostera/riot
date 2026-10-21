import { Button as ButtonPrimitive } from "@base-ui/react/button"
import { buttonVariants as kumoButtonVariants } from "@cloudflare/kumo"

import { cn } from "@/lib/utils"

/**
 * Button styles backed by Kumo.
 *
 * Keeps the shadcn-style variant and size names used across the app (and in
 * `.astro` class strings via `buttonVariants`) and maps them onto Kumo's
 * button variants so every button shares Kumo's sizing, focus, and color
 * tokens.
 */
type Variant = "default" | "outline" | "secondary" | "ghost" | "destructive" | "link"
type Size = "default" | "xs" | "sm" | "lg" | "icon" | "icon-xs" | "icon-sm" | "icon-lg"

const variantMap = {
  default: "primary",
  outline: "outline",
  secondary: "secondary",
  ghost: "ghost",
  destructive: "secondary-destructive",
  link: "ghost",
} as const

const sizeMap = {
  default: { size: "base", shape: "base" },
  xs: { size: "xs", shape: "base" },
  sm: { size: "sm", shape: "base" },
  lg: { size: "lg", shape: "base" },
  icon: { size: "base", shape: "square" },
  "icon-xs": { size: "xs", shape: "square" },
  "icon-sm": { size: "sm", shape: "square" },
  "icon-lg": { size: "lg", shape: "square" },
} as const

interface ButtonVariantOptions {
  variant?: Variant | null
  size?: Size | null
  className?: string
}

function buttonVariants({ variant, size, className }: ButtonVariantOptions = {}) {
  const resolvedSize = sizeMap[size ?? "default"]
  return cn(
    kumoButtonVariants({
      variant: variantMap[variant ?? "default"],
      size: resolvedSize.size,
      shape: resolvedSize.shape,
    }),
    // Kumo's <Button> sets these custom properties inline; class-only usage
    // (e.g. links styled as buttons in .astro files) needs them from CSS.
    (variant ?? "default") === "default" && "pkgs-button-emphasis",
    variant === "link" && "underline-offset-4 hover:underline",
    className
  )
}

function Button({
  className,
  variant = "default",
  size = "default",
  ...props
}: Omit<ButtonPrimitive.Props, "className"> & { className?: string; variant?: Variant; size?: Size }) {
  return (
    <ButtonPrimitive
      data-slot="button"
      className={buttonVariants({ variant, size, className })}
      {...props}
    />
  )
}

export { Button, buttonVariants }
