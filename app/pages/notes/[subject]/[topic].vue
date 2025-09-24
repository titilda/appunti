<template>
    <UPage>
        <UPageSection>

            <template #title>
                {{ note?.title || 'Topic Notes' }}
            </template>

            <template #description>
                {{ note?.description || 'Notes for this topic' }}
            </template>

            <UContainer class="prose dark:prose-invert">
                <ContentRenderer v-if="note" :value="note" />
            </UContainer>
        </UPageSection>
    </UPage>
</template>

<script setup lang="ts">
const route = useRoute()
const subjectSlug = route.params.subject as string
const topicSlug = route.params.topic as string

// Fetch subject data
const { data: subject } = await useAsyncData(`subject-${subjectSlug}`, () =>
    queryCollection('subject').where("id", "LIKE", `subject/${subjectSlug}/%`).first()
)

// Fetch topic data
const { data: note } = await useAsyncData(`topic-${topicSlug}`, () =>
    queryCollection('note').where("slug", "=", topicSlug).first()
)

// Set page meta
useHead({
    title: note.value?.title || 'Topic Notes',
    meta: [
        { name: 'description', content: note.value?.description || 'Browse notes for this topic' }
    ]
})
</script>