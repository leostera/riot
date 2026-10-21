import { useState } from "react";
import { Badge, LayerCard, Text } from "@cloudflare/kumo";
import { Tabs } from "@cloudflare/kumo/primitives/tabs";
import { CheckIcon } from "@phosphor-icons/react";
import { commandGroups, commandSurfaces } from "../content/commands";
import { CodeSample } from "./CodeSample";
import { InlineText } from "./InlineText";

const firstSurface = commandSurfaces[0]!.id;

/**
 * Vertical tab list of Riot command surfaces with a detail panel.
 *
 * All panels stay mounted so the copy is present in the static HTML; only the
 * selected one is visible.
 */
export function CommandExplorer() {
  const [selected, setSelected] = useState(firstSurface);

  return (
    <Tabs.Root
      value={selected}
      onValueChange={(value) => setSelected(String(value))}
      orientation="vertical"
      className="grid gap-4 lg:grid-cols-[15rem_minmax(0,1fr)] lg:gap-6"
    >
      <Tabs.List
        aria-label="Riot commands"
        activateOnFocus
        className="-mx-4 flex gap-1 overflow-x-auto px-4 pb-1 lg:mx-0 lg:flex-col lg:overflow-visible lg:px-0 lg:pb-0"
      >
        {commandGroups.map((group) => (
          <div key={group} className="contents lg:flex lg:flex-col lg:gap-0.5" role="presentation">
            <span
              role="presentation"
              className="hidden px-3 pt-4 pb-1 text-xs font-medium text-kumo-subtle first:pt-0 lg:block"
            >
              {group}
            </span>
            {commandSurfaces
              .filter((surface) => surface.group === group)
              .map((surface) => (
                <Tabs.Tab
                  key={surface.id}
                  value={surface.id}
                  className="group flex shrink-0 flex-col items-start rounded-lg px-3 py-1.5 text-left ring-kumo-hairline hover:bg-kumo-tint aria-selected:bg-kumo-base aria-selected:shadow-xs aria-selected:ring"
                >
                  <span className="font-mono text-[13px] text-kumo-default group-aria-selected:text-kumo-brand">
                    {surface.label}
                  </span>
                  <span className="hidden text-xs text-kumo-subtle lg:block">{surface.summary}</span>
                </Tabs.Tab>
              ))}
          </div>
        ))}
      </Tabs.List>

      {commandSurfaces.map((surface) => (
        <Tabs.Panel key={surface.id} value={surface.id} keepMounted className="min-w-0 focus-visible:outline-none">
          <LayerCard>
            <LayerCard.Secondary className="flex flex-wrap items-center gap-1.5">
              {surface.commands.map((command) => (
                <Badge key={command} variant="outline" className="font-mono">
                  {command}
                </Badge>
              ))}
            </LayerCard.Secondary>
            <LayerCard.Primary className="grid gap-6 px-5 py-5 md:px-6">
              <div className="grid gap-1.5">
                <Text variant="heading" size="lg" as="h3">
                  {surface.title}
                </Text>
                <Text variant="secondary">
                  <InlineText text={surface.description} />
                </Text>
              </div>

              <ul className="grid gap-2">
                {surface.points.map((point) => (
                  <li key={point} className="flex items-start gap-2 text-kumo-default">
                    <span className="flex h-lh items-center text-kumo-brand" aria-hidden="true">
                      <CheckIcon size={14} weight="bold" />
                    </span>
                    <span>
                      <InlineText text={point} />
                    </span>
                  </li>
                ))}
              </ul>

              <div className="grid gap-4 xl:grid-cols-2">
                {surface.examples.map((example) => (
                  <figure key={example.title} className="grid min-w-0 content-start gap-2">
                    <figcaption className="grid gap-0.5">
                      <Text bold>{example.title}</Text>
                      <Text variant="secondary" size="sm">
                        {example.description}
                      </Text>
                    </figcaption>
                    <CodeSample code={example.code} />
                  </figure>
                ))}
              </div>
            </LayerCard.Primary>
          </LayerCard>
        </Tabs.Panel>
      ))}
    </Tabs.Root>
  );
}
