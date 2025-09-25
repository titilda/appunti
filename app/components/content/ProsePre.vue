<template>
    <pre v-if="!isMermaid" :class="$props.class"><slot /></pre>

    <Mermaid v-else>
        {{ code }}
    </Mermaid>
</template>

<script setup lang="ts">
const props = defineProps({
    code: {
        type: String,
        default: ''
    },
    language: {
        type: String,
        default: null
    },
    filename: {
        type: String,
        default: null
    },
    highlights: {
        type: Array as () => number[],
        default: () => []
    },
    meta: {
        type: String,
        default: null
    },
    class: {
        type: String,
        default: null
    }
})

const isMermaid = ref<boolean>(props.language === 'mermaid')
</script>