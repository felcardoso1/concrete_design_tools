#!/bin/bash
#
# Build script for Concrete Design Tools
# Compiles all Pascal source files and places binaries in AGENT/ folder
#
# Uses smart linking (-XX -CX) to produce ~121KB binaries instead of ~536KB.
# This enables binaries to fit in CPU L3 cache for faster repeated execution.
# See docs/BINARY_SIZE_OPTIMIZATION.md for details.
#

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src"
AGENT_DIR="$SCRIPT_DIR/AGENT"

echo "Building Concrete Design Tools..."
echo "Source: $SRC_DIR"
echo "Output: $AGENT_DIR"
echo ""

# Ensure AGENT directory exists
mkdir -p "$AGENT_DIR"

# Count files to compile
TOTAL=$(find "$SRC_DIR" -name "*.pas" | wc -l)
COUNT=0
FAILED=0

# Compile each .pas file
for pas_file in "$SRC_DIR"/*.pas; do
    if [ -f "$pas_file" ]; then
        COUNT=$((COUNT + 1))
        filename=$(basename "$pas_file" .pas)
        echo "[$COUNT/$TOTAL] Compiling $filename..."

        if fpc -O2 -Xs -XX -CX -o"$AGENT_DIR/$filename" "$pas_file" > /dev/null 2>&1; then
            # Show binary size
            if command -v stat &> /dev/null; then
                size=$(stat --printf="%s" "$AGENT_DIR/$filename" 2>/dev/null || stat -f%z "$AGENT_DIR/$filename" 2>/dev/null)
                size_kb=$((size / 1024))
                echo "         -> $AGENT_DIR/$filename (${size_kb}KB)"
            else
                echo "         -> $AGENT_DIR/$filename"
            fi
        else
            echo "         -> FAILED"
            FAILED=$((FAILED + 1))
        fi
    fi
done

echo ""
echo "Build complete!"
echo "  Compiled: $((COUNT - FAILED))/$TOTAL"
if [ $FAILED -gt 0 ]; then
    echo "  Failed: $FAILED"
    exit 1
fi

# Clean up object files and unit cache
rm -f "$AGENT_DIR"/*.o 2>/dev/null || true
rm -f "$SRC_DIR"/*.o "$SRC_DIR"/*.ppu 2>/dev/null || true

# Make binaries executable (Linux/macOS)
chmod +x "$AGENT_DIR"/flexao "$AGENT_DIR"/flexaot "$AGENT_DIR"/flexao_verif \
         "$AGENT_DIR"/cisalhamento "$AGENT_DIR"/torcao \
         "$AGENT_DIR"/flexo_tracao "$AGENT_DIR"/flexo_compressao 2>/dev/null || true

echo ""
echo "Binaries are in: $AGENT_DIR/"
echo "To use: cd AGENT && ./flexao --help"
