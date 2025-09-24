import { defineCollection, defineContentConfig, z } from "@nuxt/content";

export default defineContentConfig({
  collections: {
    subject: defineCollection({
      type: "data",
      source: "*/*.yaml",
      schema: z.object({
        title: z.string(),
        description: z.string(),
        icon: z.string(),
      }),
    }),
    note: defineCollection({
      type: "page",
      source: "**/*.md",
      schema: z.object({
        title: z.string(),
        description: z.string().max(160),
        slug: z.string(),
        authors: z.array(z.string()),
      }),
    }),
  },
});
