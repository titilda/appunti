<template>
    <UContainer class="my-8">
        <!-- View Toggle -->
        <div class="flex justify-end mb-6">
            <UFieldGroup size="sm">
                <UButton icon="i-heroicons-squares-2x2" :variant="viewMode === 'grid' ? 'solid' : 'outline'"
                    @click="viewMode = 'grid'">
                    Grid
                </UButton>
                <UButton icon="i-heroicons-list-bullet" :variant="viewMode === 'list' ? 'solid' : 'outline'"
                    @click="viewMode = 'list'">
                    List
                </UButton>
            </UFieldGroup>
        </div>

        <!-- Grid View -->
        <div v-if="viewMode === 'grid'" class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <NuxtLink v-for="subject in subjects" :key="subject.id" :to="`/notes/${subject.id.split('/')[1]}/`"
                class="block group">
                <UCard
                    class="h-full group-hover:bg-slate-200 dark:group-hover:bg-slate-800 transition-colors duration-300">
                    <div class="flex items-center">
                        <div class="p-2 shrink-0 size-10 rounded bg-slate-300 dark:bg-slate-700">
                            <UIcon :name="subject.icon ?? 'default-icon'" class="size-6" />
                        </div>
                        <span class="ml-4 text-lg font-semibold">
                            {{ subject.title }}
                        </span>
                    </div>
                    <p class="mt-2 text-slate-500">{{ subject.description }}</p>
                </UCard>
            </NuxtLink>
        </div>

        <!-- List View -->
        <div v-else class="space-y-3">
            <NuxtLink v-for="subject in subjects" :key="subject.id" :to="`/notes/${subject.id.split('/')[1]}/`"
                class="block group">
                <UCard class="group-hover:bg-slate-200 dark:group-hover:bg-slate-800 transition-colors duration-300">
                    <div class="flex items-center justify-between">
                        <div class="flex items-center">
                            <div class="p-2 size-10 shrink-0 rounded bg-slate-300 dark:bg-slate-700">
                                <UIcon :name="subject.icon ?? 'default-icon'" class="size-6" />
                            </div>
                            <div class="ml-4">
                                <h3 class="text-lg font-semibold">{{ subject.title }}</h3>
                                <p class="text-slate-500 text-sm">{{ subject.description }}</p>
                            </div>
                        </div>
                        <UIcon name="i-heroicons-chevron-right"
                            class="size-5 text-slate-400 group-hover:text-slate-600 dark:group-hover:text-slate-300 transition-colors" />
                    </div>
                </UCard>
            </NuxtLink>
        </div>
    </UContainer>
</template>

<script setup lang="ts">
const viewMode = ref<"grid" | "list">("grid");
const { data: subjects } = await useAsyncData("subjects", () =>
    queryCollection("subject").all()
);
</script>
