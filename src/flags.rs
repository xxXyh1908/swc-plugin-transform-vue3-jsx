/// https://github.com/vuejs/vue-next/blob/master/packages/shared/src/patchFlags.ts
#[warn(non_snake_case, unused_variables)]
pub(crate) mod PatchFlags {
    #[warn(non_snake_case)]
    pub const TEXT: i32 = 1;
    #[warn(non_snake_case)]
    pub const CLASS: i32 = 1 << 1;
    #[warn(non_snake_case)]
    pub const STYLE: i32 = 1 << 2;
    #[warn(non_snake_case)]
    pub const PROPS: i32 = 1 << 3;
    #[warn(non_snake_case)]
    pub const FULL_PROPS: i32 = 1 << 4;
    #[warn(non_snake_case)]
    pub const HYDRATE_EVENTS: i32 = 1 << 5;
    #[warn(non_snake_case)]
    pub const STABLE_FRAGMENT: i32 = 1 << 6;
    #[warn(non_snake_case)]
    pub const KEYED_FRAGMENT: i32 = 1 << 7;
    #[warn(non_snake_case)]
    pub const UNKEYED_FRAGMENT: i32 = 1 << 8;
    #[warn(non_snake_case)]
    pub const NEED_PATCH: i32 = 1 << 9;
    #[warn(non_snake_case)]
    pub const DYNAMIC_SLOTS: i32 = 1 << 10;
    #[warn(non_snake_case)]
    pub const HOISTED: i32 = -1;
    #[warn(non_snake_case)]
    pub const BAIL: i32 = -2;
}

/// https://github.com/vuejs/vue-next/blob/master/packages/shared/src/slotFlags.ts
#[warn(non_snake_case)]
pub(crate) mod SlotFlags {
    ///
    /// Stable slots that only reference slot props or context state. The slot
    /// can fully capture its own dependencies so when passed down the parent won't
    /// need to force the child to update.
    ///
    #[warn(non_snake_case)]
    pub const STABLE: i32 = 1;
    ///
    /// Slots that reference scope variables (v-for or an outer slot prop), or
    /// has conditional structure (v-if, v-for). The parent will need to force
    /// the child to update because the slot does not fully capture its dependencies.
    ///
    #[warn(non_snake_case)]
    pub const DYNAMIC: i32 = 2;
    ///
    /// `<slot/>` being forwarded into a child component. Whether the parent needs
    /// to update the child is dependent on what kind of slots the parent itself
    /// received. This has to be refined at runtime, when the child's vnode
    /// is being created (in `normalizeChildren`)
    ///
    #[warn(non_snake_case)]
    pub const FORWARDED: i32 = 3;
}
