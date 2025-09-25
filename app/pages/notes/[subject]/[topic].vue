<template>
    <UPage>
        <template #left>
            <UPageAside>
                <UContentNavigation :navigation="navigation" class="pl-4 pt-8" />
            </UPageAside>
        </template>
        <UPageSection>
            <template #title>
                {{ note?.title || 'Topic Notes' }}
            </template>

            <template #description>
                {{ note?.description || 'Notes for this topic' }}
            </template>

            <UContainer>
                <ContentRenderer v-if="note" :value="note" class="prose dark:prose-invert" />

                <USeparator class="mt-8" />

                <div class="grid gap-8 sm:grid-cols-2 mt-8">
                    <NuxtLink v-if="previousNote" :to="`/notes/${subjectSlug}/${previousNote.slug}`"
                        class="block text-left text-decoration-none">
                        <UCard>
                            <div class="flex items-center gap-2">
                                <UIcon name="i-heroicons-chevron-left" class="size-8" />
                                <div class="overflow-hidden">
                                    <span class="block truncate">
                                        {{ previousNote?.title }}
                                    </span>
                                    <span class="block truncate text-sm text-gray-500">
                                        {{ previousNote?.description }}
                                    </span>
                                </div>
                            </div>
                        </UCard>
                    </NuxtLink>
                    <span v-else class="hidden sm:block">&nbsp;</span>

                    <NuxtLink v-if="nextNote" :to="`/notes/${subjectSlug}/${nextNote.slug}`" class="block text-right">
                        <UCard>
                            <div class="flex items-center gap-2 justify-end">
                                <div class="overflow-hidden">
                                    <span class="block truncate">
                                        {{ nextNote?.title }}
                                    </span>
                                    <span class="block truncate text-sm text-gray-500">
                                        {{ nextNote?.description }}
                                    </span>
                                </div>
                                <UIcon name="i-heroicons-chevron-right" class="size-8" />
                            </div>
                        </UCard>
                    </NuxtLink>
                </div>
            </UContainer>
        </UPageSection>
        <template #right>
            <UPageAside class="pr-4">
                <UContentToc :title="note?.title" highlight :links="note?.body?.toc?.links" />
            </UPageAside>
        </template>
    </UPage>
</template>

<script setup lang="ts">
const route = useRoute()
const subjectSlug = route.params.subject as string
const topicSlug = route.params.topic as string

// Fetch notes for the subject
const { data: notes } = await useAsyncData(`notes-${subjectSlug}`, () =>
    queryCollection('note').where("id", "LIKE", `note/${subjectSlug}/%`).all()
)

// Get current note
const note = computed(() => {
    return notes.value?.find(n => n.slug === topicSlug) || null
})

const navigation = computed(() => {
    return notes.value?.map(n => ({
        title: n.title,
        href: `/notes/${subjectSlug}/${n.slug}`,
        active: n.slug === topicSlug
    })) || []
})

// Get previous and next notes
const currentIndex = computed(() => {
    return notes.value?.findIndex(n => n.slug === topicSlug) ?? -1
})

const previousNote = computed(() => {
    if (notes.value && currentIndex.value > 0) {
        return notes.value[currentIndex.value - 1]
    }

    return null
})

const nextNote = computed(() => {
    if (notes.value && currentIndex.value < notes.value.length - 1) {
        return notes.value[currentIndex.value + 1]
    }

    return null
})

// Set page meta
useHead({
    title: note.value?.title || 'Topic Notes',
    meta: [
        { name: 'description', content: note.value?.description || 'Browse notes for this topic' }
    ]
})
</script>