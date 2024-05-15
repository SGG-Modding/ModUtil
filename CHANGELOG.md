# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.1.0] - 2024-05-15

### Added

- Added a callback to wait for the end of all game scripts (easier to remember than explicitly queueing onto `RoomLogic.lua`)

### Changed

- Changed the structure of the definitions files, now `modutil.mod` has some hints (still a work in progress).

### Fixed

- Fixed `ModUtil.Hades.PrintStack`, as it would read the config incorrectly and try to use a since deleted internal table.

## [3.0.0] - 2024-05-15

### Added

- Added interface as a ReturnOfModding plugin, with helpers to manage loading (e.g. on_ready_early)

### Changed

- Moved ModUtil.Mod.Register and ModData to ModUtil.lua so it will be available earlier in the load order
- Moved ModUtil.Hades.PrintStack config to dedicated module wide config
- Moved to ReturnOfModding layout (still works on ModImporter)

### Removed

- Removed all menu handling capabilities from ModUtil.Hades
- Removed ModUtil.Compat, which includes the old names, such as WrapBaseFunction

## [2.10.1] - 2024-04-24

### Added

- Initial Thunderstore release.

[unreleased]: https://github.com/SGG-Modding/ModUtil/compare/3.1.0...HEAD
[3.1.0]: https://github.com/SGG-Modding/ModUtil/compare/3.0.0...3.1.0
[3.0.0]: https://github.com/SGG-Modding/ModUtil/compare/2.10.1...3.0.0
[2.10.1]: https://github.com/SGG-Modding/ModUtil/compare/d8c6fac014fa9ff3ac2bcc38b5505c9bae1e71f7...2.10.1
