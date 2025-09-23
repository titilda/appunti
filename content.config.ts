import { defineCollection, defineContentConfig, z } from "@nuxt/content";

export default defineContentConfig({
  collections: {
    subject: defineCollection({
      type: "data",
      source: "subjects/*/*.yaml",
      schema: z.object({
        title: z.string().optional(),
        description: z.string().optional(),
        color: z.string().optional(),
        icon: z.string().optional(),
      }),
    }),
    notes: defineCollection({
      type: "page",
      source: "subjects/**/*.md",
      schema: z.object({
        authors: z.array(z.string()).optional(),
      }),
    }),
  },
});
