# Quirky Test Suite

This directory contains comprehensive tests for the Quirky backend.

## Running Tests

### Run all tests:
```bash
cabal test
```

### Run tests with verbose output:
```bash
cabal test --test-show-details=direct
```

### Run specific test modules (after building):
```bash
cabal test --test-option=--pattern="Config Parsing"
```

## Test Structure

### ConfigParsingTests
Tests YAML/JSON config parsing for all configuration types:
- HealthStatus parsing
- ActionType parsing (builtin, named, composite)
- HealthCheck parsing
- SatelliteConfig parsing
- AggregatorConfig parsing
- Top-level Config parsing
- RunMode detection

### TemplatingTests
Tests template substitution engine:
- Simple template substitution (`{{ variable }}`)
- Nested path lookups (`{{ user.name }}`)
- JSON reference injection (`json_ref`)
- Array templating
- Edge cases (malformed templates, missing variables)

### ActionTests
Tests action execution framework:
- HealthResult creation and validation
- ActionError types
- Error handling

### BuiltinActionTests
Tests all builtin actions:
- **ParseJson**: JSON parsing and validation
- **JsonExtract**: Path-based value extraction from JSON
- **ParseHtml**: HTML parsing and element selection
- **RegexMatch**: Pattern matching with capture groups
- **CompareNumber**: Numeric threshold comparisons
- **FileStat**: File existence and metadata checks

### AlertTests
Tests alert logic:
- `shouldAlert` decision logic
- Alert message building
- Failure collection from satellites
- Duplicate alert prevention

## Test Fixtures

The `fixtures/` directory contains example configuration files:
- `example-satellite.yaml`: Sample satellite configuration
- `example-aggregator.yaml`: Sample aggregator configuration

These can be used for manual testing and integration testing.

## Adding New Tests

When adding new tests:

1. Create a new test module in `tests/`
2. Add the module to `quirky.cabal` under `other-modules`
3. Import and add to `tests/Main.hs`
4. Follow the existing patterns for test organization

## Test Coverage

Current test coverage includes:
- ✅ Configuration parsing (all types)
- ✅ Template engine
- ✅ Builtin actions (most)
- ✅ Alert logic
- ⚠️ HTTP actions (mocked/limited)
- ❌ External action execution (requires test fixtures)
- ❌ Full end-to-end checks
- ❌ Aggregator polling loop
- ❌ Dashboard HTML generation

## Dependencies

Test dependencies are managed in `quirky.cabal`:
- `tasty`: Main test framework
- `tasty-hunit`: Unit test assertions
- `tasty-quickcheck`: Property-based testing
- `aeson-qq`: JSON quasiquotes for test data
