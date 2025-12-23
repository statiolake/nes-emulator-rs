#!/usr/bin/env python3
import re

# アドレッシングモードのマッピング
ADDRESSING_MODE_MAP = {
    "Implied": "AddressingMode::Implied",
    "Immediate": "AddressingMode::Immediate",
    "Zero Page": "AddressingMode::ZeroPage",
    "Zero Page,X": "AddressingMode::ZeroPageX",
    "Zero Page,Y": "AddressingMode::ZeroPageY",
    "Absolute": "AddressingMode::Absolute",
    "Absolute,X": "AddressingMode::AbsoluteX",
    "Absolute,Y": "AddressingMode::AbsoluteY",
    "(Indirect,X)": "AddressingMode::IndirectIndexed",
    "(Indirect),Y": "AddressingMode::IndexedIndirect",
}

def parse_opcodes(filename):
    """ファイルをパースしてオペコード情報を抽出"""
    opcodes = []

    with open(filename, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # 既知のニモニック
    known_mnemonics = {
        'AAC', 'AAX', 'ARR', 'ASR', 'ATX', 'AXA', 'AXS', 'DCP', 'DOP', 'ISC',
        'KIL', 'LAR', 'LAX', 'NOP', 'RLA', 'RRA', 'SBC', 'SLO', 'SRE', 'SXA',
        'SYA', 'TOP', 'XAA', 'XAS'
    }

    i = 0
    while i < len(lines):
        line = lines[i]

        # オペコード名セクション（例：AAC (ANC) [ANC]）を検出
        # 最初の単語がニモニックかどうかでチェック
        first_word = line.strip().split()[0] if line.strip() else ""
        if first_word in known_mnemonics and '(' in line:
            mnemonic = first_word

            # テーブルを見つけるまでスキップ
            i += 1
            while i < len(lines):
                if '|Opc|' in lines[i] or '|Sz|' in lines[i]:
                    # テーブルヘッダを見つけた
                    i += 2  # ヘッダとセパレータをスキップ
                    break
                i += 1

            # テーブルの行をパース
            while i < len(lines):
                line = lines[i].strip()

                # テーブルの終わりを検出
                if not line or '---' in line:
                    i += 1
                    break

                # 新しいセクションの開始（=記号）を検出
                if '=' in line:
                    break

                # テーブル行をパース（例：Zero Page   |AAX arg    |$87| 2 | 3）
                if '|' in line:
                    parts = [p.strip() for p in line.split('|')]
                    if len(parts) >= 5 and parts[0] and parts[0] in ADDRESSING_MODE_MAP:
                        addressing_mode_str = parts[0]
                        opcode_hex = parts[2]
                        size = parts[3]
                        cycles_raw = parts[4].rstrip('*').strip()

                        # アドレッシングモードをマッピング
                        addressing_mode = ADDRESSING_MODE_MAP[addressing_mode_str]

                        # オペコードをデコード（$87 -> 0x87）
                        if opcode_hex.startswith('$'):
                            opcode_clean = opcode_hex.lstrip('$')
                            opcode_int = int(opcode_clean, 16)

                            # サイクルが不明な場合（-）は0に設定
                            cycles_int = 0 if cycles_raw == '-' else int(cycles_raw)

                            opcodes.append({
                                'opcode': opcode_int,
                                'mnemonic': mnemonic,
                                'addressing_mode': addressing_mode,
                                'size': int(size),
                                'cycles': cycles_int,
                            })

                i += 1
        else:
            i += 1

    return opcodes

def generate_rust_code(opcodes):
    """Rustコード片を生成"""
    lines = []

    for opcode in opcodes:
        method_name = opcode['mnemonic'].lower()
        line = f"    Opcode::new_unofficial(0x{opcode['opcode']:02X}, \"{opcode['mnemonic']}\", {opcode['addressing_mode']}, {opcode['cycles']}, Cpu::{method_name}),"
        lines.append(line)

    return '\n'.join(lines)

if __name__ == '__main__':
    opcodes = parse_opcodes('references/unofficial_opcodes.txt')

    # オペコード順にソート
    opcodes.sort(key=lambda x: x['opcode'])

    rust_code = generate_rust_code(opcodes)
    print(rust_code)
