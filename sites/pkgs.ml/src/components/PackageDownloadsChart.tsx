import type { PackageStackedDownloadSeries } from "@/lib/types"
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
  stackedDownloads: PackageStackedDownloadSeries[]
}

const date = new Intl.DateTimeFormat("en", {
  month: "short",
  day: "numeric",
  timeZone: "UTC",
})
const number = new Intl.NumberFormat("en-US")
const seriesColors = [
  "var(--chart-5)",
  "var(--chart-4)",
  "var(--chart-3)",
  "var(--chart-2)",
  "var(--chart-1)",
  "var(--muted-foreground)",
]

function formatDay(value: string): string {
  const [year, month, dayOfMonth] = value.split("-").map(Number)
  return date.format(new Date(Date.UTC(year, (month ?? 1) - 1, dayOfMonth ?? 1)))
}

function formatCompactNumber(value: number): string {
  if (value >= 1_000) {
    return `${Math.round(value / 100) / 10}k`
  }

  return number.format(value)
}

export default function PackageDownloadsChart({ stackedDownloads }: Props) {
  const otherSeries = stackedDownloads.find((series) => series.is_other)
  const versionSeries = stackedDownloads.filter((series) => !series.is_other)
  const orderedSeries = [
    ...(otherSeries ? [otherSeries] : []),
    ...[...versionSeries].reverse(),
  ]
  const legendSeries = [
    ...versionSeries,
    ...(otherSeries ? [otherSeries] : []),
  ]
  const chartData = orderedSeries[0]?.daily_downloads.map((point, index) => {
    const row: Record<string, number | string> = {
      date: point.date,
    }

    for (const series of orderedSeries) {
      row[series.key] = series.daily_downloads[index]?.download_count ?? 0
    }

    return row
  }) ?? []
  const colorBySeries = new Map(
    orderedSeries.map((series, index) => [series.key, seriesColors[index] ?? seriesColors[seriesColors.length - 1]])
  )

  return (
    <div className="grid gap-3">
      <div className="grid gap-1">
        <div className="text-sm text-muted-foreground">Downloads over the last 30 days</div>
        <div className="flex flex-wrap items-center gap-x-4 gap-y-2 text-sm">
          {legendSeries.map((series) => (
            <div key={series.key} className="flex items-center gap-2 text-foreground">
              <span
                className="h-2.5 w-2.5 rounded-full"
                style={{ backgroundColor: colorBySeries.get(series.key) ?? seriesColors[0] }}
              />
              <span>{series.is_other ? series.label : `v${series.label}`}</span>
              <span className="text-muted-foreground">{number.format(series.total_downloads)}</span>
            </div>
          ))}
        </div>
      </div>

      <div className="h-72 w-full">
        <ResponsiveContainer width="100%" height="100%">
          <AreaChart data={chartData} margin={{ top: 12, right: 8, left: 8, bottom: 0 }}>
            <CartesianGrid stroke="var(--border)" vertical={false} />
            <XAxis
              dataKey="date"
              tickFormatter={formatDay}
              tickLine={false}
              axisLine={false}
              minTickGap={24}
              tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
            />
            <YAxis
              allowDecimals={false}
              tickFormatter={formatCompactNumber}
              tickLine={false}
              axisLine={false}
              width={40}
              tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
            />
            <Tooltip
              formatter={(value, name) => [number.format(Number(value ?? 0)), name === "other" ? "Other" : `v${String(name)}`]}
              labelFormatter={(value) => formatDay(String(value))}
              contentStyle={{
                borderRadius: "0.45rem",
                borderColor: "var(--border)",
                backgroundColor: "var(--card)",
              }}
            />
            {orderedSeries.map((series) => (
              <Area
                key={series.key}
                type="monotone"
                dataKey={series.key}
                name={series.key}
                stackId="downloads"
                stroke={colorBySeries.get(series.key) ?? seriesColors[0]}
                fill={colorBySeries.get(series.key) ?? seriesColors[0]}
                fillOpacity={series.is_other ? 0.2 : 0.3}
                strokeWidth={1.75}
                dot={false}
                activeDot={{ r: 3 }}
                isAnimationActive={false}
              />
            ))}
          </AreaChart>
        </ResponsiveContainer>
      </div>
    </div>
  )
}
