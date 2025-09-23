<template>
    <div class="min-h-screen flex flex-col">
        <UHeader title="TiTilda Appunti" to="/">
            <template #title>
                <span class="flex items-center gap-2">
                    <img class="h-6 w-auto" src="/logo.svg" alt="TiTilda Logo">
                    TiTilda Appunti
                </span>
            </template>
        </UHeader>
        <div class="flex-grow">
            <slot />
        </div>
        <UFooter>
            <template #left>
            <p class="text-muted text-sm">Copyright © {{ new Date().getFullYear() }}</p>
            </template>
      </UFooter>
    </div>
</template>