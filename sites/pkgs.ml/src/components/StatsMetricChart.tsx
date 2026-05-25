import type { RegistryStatsActivityPoint, RegistryStatsMetricSeries, RegistryStatsWindowKey } from "@/lib/types"
import {
  Area,
  AreaChart,
  CartesianGrid,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis,
} from "recharts"

interface Props {
  metric: RegistryStatsMetricSeries
  window: RegistryStatsWindowKey
}

const compactNumber = new Intl.NumberFormat("en-US", {
  notation: "compact",
  maximumFractionDigits: 1,
})

const fullNumber = new Intl.NumberFormat("en-US")

const dayFormatter = new Intl.DateTimeFormat("en", {
  month: "short",
  day: "numeric",
  timeZone: "UTC",
})

const monthFormatter = new Intl.DateTimeFormat("en", {
  month: "short",
  timeZone: "UTC",
})

const monthYearFormatter = new Intl.DateTimeFormat("en", {
  month: "short",
  year: "numeric",
  timeZone: "UTC",
})

function parseBucket(value: string): Date {
  return value.length === 10 ? new Date(`${value}T00:00:00Z`) : new Date(value)
}

function formatBucket(value: string, window: RegistryStatsWindowKey): string {
  const date = parseBucket(value)

  switch (window) {
    case "year":
      return monthFormatter.format(date)
    case "all":
      return monthYearFormatter.format(date)
    default:
      return dayFormatter.format(date)
  }
}

function formatTooltipLabel(value: string, window: RegistryStatsWindowKey): string {
  const date = parseBucket(value)

  switch (window) {
    case "all":
      return monthYearFormatter.format(date)
    default:
      return date.toLocaleDateString("en-US", {
        month: "short",
        day: "numeric",
        year: "numeric",
        timeZone: "UTC",
      })
  }
}

function formatAxisNumber(value: number): string {
  if (value === 0) {
    return "0"
  }

  return compactNumber.format(value)
}

export default function StatsMetricChart({ metric, window }: Props) {
  return (
    <div className="grid gap-4">
      <div className="flex items-start justify-between gap-3">
        <div className="grid gap-1">
          <div className="text-xs uppercase tracking-wide text-muted-foreground">Window total</div>
        </div>
        <div className="text-right">
          <div className="text-lg font-semibold tracking-tight text-foreground">
            {fullNumber.format(metric.total)}
          </div>
        </div>
      </div>

      <div className="h-56 w-full">
        <ResponsiveContainer width="100%" height="100%">
          <AreaChart data={metric.points} margin={{ top: 8, right: 8, left: 0, bottom: 0 }}>
            <CartesianGrid stroke="var(--border)" vertical={false} />
            <XAxis
              dataKey="date"
              tickFormatter={(value) => formatBucket(String(value), window)}
              tickLine={false}
              axisLine={false}
              minTickGap={24}
              tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
            />
            <YAxis
              allowDecimals={false}
              tickFormatter={formatAxisNumber}
              tickLine={false}
              axisLine={false}
              width={42}
              tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
            />
            <Tooltip
              content={({ active, label, payload }) => {
                if (!active || label === undefined || payload === undefined || payload.length === 0) {
                  return null
                }

                const item = payload[0]
                const numeric = typeof item?.value === "number" ? item.value : Number(item?.value ?? 0)

                return (
                  <div className="min-w-36 rounded-md border border-border bg-background px-3 py-2 text-sm shadow-sm">
                    <div className="mb-2 font-medium text-foreground">
                      {formatTooltipLabel(String(label), window)}
                    </div>
                    <div className="flex items-center justify-between gap-3">
                      <div className="flex items-center gap-2 text-muted-foreground">
                        <span
                          className="inline-block h-2 w-2 rounded-full"
                          style={{ backgroundColor: metric.color }}
                        />
                        <span>{metric.label}</span>
                      </div>
                      <span className="font-medium text-foreground">{fullNumber.format(numeric)}</span>
                    </div>
                  </div>
                )
              }}
            />
            <Area
              type="monotone"
              dataKey={metric.key as keyof RegistryStatsActivityPoint}
              name={metric.label}
              stroke={metric.color}
              fill={metric.color}
              fillOpacity={0.22}
              strokeWidth={2}
              dot={false}
              activeDot={{ r: 3 }}
              isAnimationActive={false}
            />
          </AreaChart>
        </ResponsiveContainer>
      </div>
    </div>
  )
}
