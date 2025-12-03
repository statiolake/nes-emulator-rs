#!/usr/bin/env python3
"""
Parse NES opcodes from HTML and generate Rust OpCode array.
Uses regex-based parsing.
"""

import re
import sys
from typing import Dict, List

# Addressing mode mapping - maps HTML descriptions to Rust enum names
# Order matters: more specific patterns first to avoid partial matches
ADDRESSING_MODES = {
    "Zero Page,X": "ZeroPageX",
    "Zero Page,Y": "ZeroPageY",
    "Absolute,X": "AbsoluteX",
    "Absolute,Y": "AbsoluteY",
    "(Indirect,X)": "IndexedIndirect",
    "(Indirect),Y": "IndirectIndexed",
    "Immediate": "Immediate",
    "Zero Page": "ZeroPage",
    "Absolute": "Absolute",
    "Indirect": "Indirect",
    "Relative": "Relative",
    "Accumulator": "Accumulator",
    "Implied": "Implied",
}

# Abbreviation to enum name mapping
ABBREVIATION_MAP = {
    "IMM": "Immediate",
    "ZPG": "ZeroPage",
    "ZPX": "ZeroPageX",
    "ZPY": "ZeroPageY",
    "ABS": "Absolute",
    "ABX": "AbsoluteX",
    "ABY": "AbsoluteY",
    "IDX": "IndexedIndirect",
    "IDY": "IndirectIndexed",
    "IND": "Indirect",
    "REL": "Relative",
    "ACC": "Accumulator",
    "IMP": "Implied",
}


def clean_html(text: str) -> str:
    """Remove HTML tags and extra whitespace."""
    # Remove HTML tags
    text = re.sub(r"<[^>]+>", "", text)
    # Remove extra whitespace
    text = re.sub(r"\s+", " ", text)
    return text.strip()


def parse_opcodes(html_content: str) -> List[Dict]:
    """Parse opcodes from HTML content using regex."""
    opcodes = []

    # Find all instruction sections marked by <h3><a name="XXX"></a>XXX</h3>
    instruction_pattern = r'<h3><a name="([A-Z]+)"></a>'
    instruction_matches = list(re.finditer(instruction_pattern, html_content))

    for i, match in enumerate(instruction_matches):
        instruction = match.group(1)
        start_pos = match.start()

        # Find the next instruction
        if i + 1 < len(instruction_matches):
            end_pos = instruction_matches[i + 1].start()
        else:
            end_pos = len(html_content)

        section = html_content[start_pos:end_pos]

        # Find the opcode table - it's the second table in the section
        # (first table is for processor status, second is for opcodes)
        table_pattern = r"<table[^>]*>.*?</table>"
        tables = re.findall(table_pattern, section, re.DOTALL)

        if len(tables) < 2:
            continue

        # The second table contains the opcodes
        table = tables[1]

        # Parse table rows
        row_pattern = r"<tr[^>]*>(.*?)</tr>"
        rows = re.findall(row_pattern, table, re.DOTALL)

        for row in rows[1:]:  # Skip header row
            # Extract cells
            cell_pattern = r"<td[^>]*>(.*?)</td>"
            cells = re.findall(cell_pattern, row, re.DOTALL)

            if len(cells) < 4:
                continue

            # Extract and clean content from cells
            addressing_mode_html = cells[0]
            opcode_text = clean_html(cells[1])
            cycles_text = clean_html(cells[3])

            # Parse opcode (format: $XX)
            opcode_match = re.search(r"\$([0-9A-Fa-f]+)", opcode_text)
            if not opcode_match:
                continue

            opcode_hex = opcode_match.group(1).upper()

            # Extract clean text from addressing mode cell (most reliable)
            addressing_mode_text = clean_html(addressing_mode_html)
            addressing_mode = None

            # First try to match by text (handles cases like "Accumulator" even if link is #IMP)
            for mode_key, mode_value in ADDRESSING_MODES.items():
                if mode_key in addressing_mode_text:
                    addressing_mode = mode_value
                    break

            # Fallback to link abbreviation if text matching failed
            if not addressing_mode:
                link_match = re.search(r'href="[^"]*#([A-Z]+)"', addressing_mode_html)
                if link_match:
                    mode_abbr = link_match.group(1)
                    addressing_mode = ABBREVIATION_MAP.get(mode_abbr)

            if not addressing_mode:
                continue

            # Parse cycles (format: "2" or "4 (+1 if page crossed)")
            cycles_match = re.search(r"(\d+)", cycles_text)
            cycles = int(cycles_match.group(1)) if cycles_match else 0

            opcodes.append(
                {
                    "instruction": instruction,
                    "addressing_mode": addressing_mode,
                    "opcode": opcode_hex,
                    "cycles": cycles,
                }
            )

    return opcodes


def format_opcode_array(opcodes: List[Dict]) -> str:
    """Format opcodes as Rust array."""
    lines = ["&["]

    for op in opcodes:
        instruction = op["instruction"]
        addressing_mode = op["addressing_mode"]
        opcode_hex = op["opcode"]
        cycles = op["cycles"]

        # Convert instruction name to lowercase for function name
        fn_name = instruction.lower()

        line = f'    Op::new(0x{opcode_hex}, "{instruction}", AddressingMode::{addressing_mode}, {cycles}, Cpu::{fn_name}),'
        lines.append(line)

    lines.append("]")
    return "\n".join(lines)


def main():
    # Read from file or stdin
    if len(sys.argv) > 1:
        with open(sys.argv[1], "r", encoding="iso-8859-1") as f:
            html_content = f.read()
    else:
        html_content = sys.stdin.read()

    # Parse opcodes
    opcodes = parse_opcodes(html_content)

    # Sort by instruction name, then opcode value
    opcodes.sort(key=lambda x: (x["instruction"], int(x["opcode"], 16)))

    # Output result
    result = format_opcode_array(opcodes)
    print(result)


if __name__ == "__main__":
    main()
