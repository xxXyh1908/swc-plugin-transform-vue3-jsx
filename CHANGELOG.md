# 0.1.0 (2022-09-16)
initial

# 0.1.2 (2022-09-16)
### Bug Fixes
- Fixed the bug of directive-argument in camelCase(.e.g `v-camel-case` => resolveDirective('`camelCase`')).

### New Features
- New Option `transformVSlot`: To convert any property that looks like `v-slot:xxx` to an `v-slots={"xxx": ...}` property.
