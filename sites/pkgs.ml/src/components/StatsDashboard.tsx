import { useState } from "react"

import type { RegistryStatsDashboardDocument, RegistryStatsWindowKey } from "@/lib/types"
import StatsMetricChart from "@/components/StatsMetricChart"
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs } from "@cloudflare/kumo"

interface Props {
  initialDashboards: Record<RegistryStatsWindowKey, RegistryStatsDashboardDocument>
}

export default function StatsDashboard({ initialDashboards }: Props) {
  const [selectedWindow, setSelectedWindow] = useState<RegistryStatsWindowKey>("30d")
  const dashboard = initialDashboards[selectedWindow] ?? initialDashboards["30d"]

  return (
    <div className="grid gap-4">
      <div className="flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between">
        <div className="grid gap-1">
          <h2 className="text-base font-semibold text-kumo-strong">{dashboard.window_label}</h2>
          <p className="text-kumo-subtle">
            One chart per metric so each trend keeps its own scale.
          </p>
        </div>

        <div className="-mx-1 overflow-x-auto px-1">
          <Tabs
            variant="segmented"
            value={selectedWindow}
            onValueChange={(value) => setSelectedWindow(value as RegistryStatsWindowKey)}
            tabs={dashboard.available_windows.map((option) => ({ value: option.key, label: option.label }))}
          />
        </div>
      </div>

      <div className="grid gap-4 xl:grid-cols-2">
        {dashboard.metrics.map((metric) => (
          <Card key={metric.key} size="sm" className="rounded-md shadow-none">
            <CardHeader className="border-b border-border pb-3">
              <CardTitle className="text-base">{metric.label}</CardTitle>
            </CardHeader>
            <CardContent className="px-3 py-3 sm:px-4">
              <StatsMetricChart metric={metric} window={dashboard.window} />
            </CardContent>
          </Card>
        ))}
      </div>
    </div>
  )
}
