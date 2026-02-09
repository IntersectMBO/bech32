# Changelog

<!-- This ChangeLog follows a format specified by: https://keepachangelog.com/en/1.0.0/ -->

## [1.1.10] - 2026-01-30

- Added support for GHC 9.14 series.
- Revised upper version bounds for dependencies.

## [1.1.9] - 2025-06-05

- Revised upper version bounds for dependencies.

## [1.1.8] - 2024-12-27

- Added support for GHC 9.12 series.

## [1.1.7] - 2024-05-20

- Added support for GHC 9.10 series.

## [1.1.6] - 2024-04-24

### Changed

- Revised upper version bounds for dependencies.

## [1.1.5] - 2024-02-23

### Fixed

- Specified version bounds for all package dependencies.

### Added

- Added support for GHC 9.8 series.

## [1.1.4] - 2023-07-28

### Fixed

- Support for `optparse-applicative` versions >= 0.18.0.0.

## [1.1.3] - 2023-06-06

### Fixed

- Specify supported version bounds for `optparse-applicative`.

## [1.1.2] - 2021-11-05

### Fixed

- Strip whitespaces from bech32 stdin to satisfy Windows `echo` command.

## [1.1.1] - 2021-06-11

### Added

- Added `--version` switch for the `bech32` command.

### Changed

- Upgraded CI to build with Cabal 3.4.0.0 and GHC 8.10.4.
- Update version constraints for GHC 9.0.1.

## [1.1.0] - 2020-07-08

### Added

- Added `bech32` command-line for easy conversions in the console.

  ```console
  Usage: bech32 [PREFIX]
    Convert to and from bech32 strings. Data are read from standard input.

  Available options:
    -h,--help                Show this help text
    PREFIX                   An optional human-readable prefix (e.g. 'addr').
                               - When provided, the input text is decoded from various encoding formats and re-encoded to bech32 using the given prefix.
                               - When omitted, the input text is decoded from bech32 to base16.

  Supported encoding formats: Base16, Bech32 & Base58.

  Examples:
    To Bech32:
      $ bech32 base16_ <<< 706174617465
      base16_1wpshgct5v5r5mxh0

      $ bech32 base58_ <<< Ae2tdPwUPEYy
      base58_1p58rejhd9592uusa8pzj2

      $ bech32 new_prefix <<< old_prefix1wpshgcg2s33x3
      new_prefix1wpshgcgeak9mv

    From Bech32:
      $ bech32 <<< base16_1wpshgct5v5r5mxh0
      706174617465
  ```

## [1.0.2] - 2020-02-19

### Added

- Added support for the `bech32-th` extension library.

## [1.0.1] - 2020-02-13

### Added

- Exposed functions `dataPartFromWords` and `dataPartToWords` within public
  interface.

- Exposed the `Word5` type within the public interface.

- Exposed the `CharPosition` type within the public interface.

### Changed

- Improved module documentation, adding basic examples to help beginner users
  quickly get up to speed.

## [1.0.0] - 2019-09-27

### Added

- Initial release pulled from https://github.com/input-output-hk/cardano-wallet
