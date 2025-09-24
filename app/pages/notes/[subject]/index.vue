<template>
    <UPage>
        <UPageSection>
            <template #title>
                {{ subject?.title || 'Subject Notes' }}
            </template>

            <template #description>
                {{ subject?.description || 'Browse all notes for this subject' }}
            </template>

            <div v-if="notes && notes.length > 0" class="space-y-4">
                <NuxtLink v-for="note in notes" :key="note.id" :to="`/notes/${subjectSlug}/${note.slug}`" class="block">
                    <UCard class="cursor-pointer hover:shadow-lg transition-shadow">

                        <h3 class="text-lg font-semibold">
                            {{ note.title }}
                        </h3>

                        <p class="text-sm text-slate-500 line-clamp-3">
                            {{ note.description || 'No description available' }}
                        </p>

                        <template #footer>
                            <div class="flex items-center justify-between text-xs text-gray-500 dark:text-gray-400">
                                <span>
                                    By {{ note.authors?.join(', ') }}
                                </span>
                            </div>
                        </template>
                    </UCard>
                </NuxtLink>
            </div>

            <div v-else class="text-center py-12">
                <UIcon name="i-lucide-file-x" class="mx-auto h-12 w-12 text-gray-400" />
                <h3 class="mt-2 text-sm font-semibold text-gray-900 dark:text-white">No notes found</h3>
                <p class="mt-1 text-sm text-gray-500 dark:text-gray-400">
                    There are no notes available for this subject yet.
                </p>
            </div>
        </UPageSection>
    </UPage>
</template>

<script setup lang="ts">
const route = useRoute()
const subjectSlug = route.params.subject as string

// Fetch subject metadata
const { data: subject } = await useAsyncData(`subject-${subjectSlug}`, () =>
    queryCollection('subject').where("id", "LIKE", `subject/${subjectSlug}/%`).first()
)

// Fetch notes for the subject
const { data: notes } = await useAsyncData(`notes-${subjectSlug}`, () =>
    queryCollection('note').where("id", "LIKE", `note/${subjectSlug}/%`).all()
)

// Set page meta
useHead({
    title: subject.value?.title ? `${subject.value.title} - Notes` : 'Subject Notes'
})
</script>
