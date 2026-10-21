import * as React from "react"
import { Input as InputPrimitive } from "@base-ui/react/input"
import { inputVariants } from "@cloudflare/kumo"

import { cn } from "@/lib/utils"

/** Text input styled with Kumo's input variants. */
function Input({
  className,
  class: astroClass,
  type,
  ...props
}: React.ComponentProps<"input"> & { class?: string }) {
  return (
    <InputPrimitive
      type={type}
      data-slot="input"
      className={cn(inputVariants({ size: "base" }), "w-full min-w-0", astroClass, className)}
      {...props}
    />
  )
}

export { Input }
