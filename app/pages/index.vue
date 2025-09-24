<template>
    <UContainer class="my-8">
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <NuxtLink v-for="subject in subjects" :key="subject.id" :to="`/`" class="group hover:shadow-lg transition-shadow duration-300">
                <UCard class="h-full group-hover:bg-slate-200 dark:group-hover:bg-slate-800 transition-colors duration-300">
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
    </UContainer>
</template>

<script setup lang="ts">
const { data: subjects } = await useAsyncData('subjects', () => queryCollection('subject').all())
</script>
