use anyhow::bail;
use log::warn;

use crate::hardware::bus::Peripheral;

pub struct Rom {
    prg_rom_size: PrgRomSize,
    prg_rom_data: Vec<u8>,
    _chr_rom_size: ChrRomSize,
    _chr_rom_data: Vec<u8>,
    mapper: Mapper,
    _screen_mirroring: ScreenMirroring,
}

impl Rom {
    pub fn parse(raw: &[u8]) -> anyhow::Result<Rom> {
        check_nes_header(raw)?;
        check_ines_version(raw)?;
        let mapper = Mapper::parse(raw)?;
        let mirroring = ScreenMirroring::parse(raw)?;
        let (prg_rom_size, prg_rom_data, chr_rom_size, chr_rom_data) = parse_prg_chr_rom(raw)?;

        Ok(Rom {
            prg_rom_size,
            prg_rom_data,
            _chr_rom_size: chr_rom_size,
            _chr_rom_data: chr_rom_data,
            mapper,
            _screen_mirroring: mirroring,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrgRomSize {
    Size16KB,
    Size32KB,
}

impl PrgRomSize {
    pub fn parse(raw: &[u8]) -> anyhow::Result<Self> {
        match raw[4] {
            1 => Ok(PrgRomSize::Size16KB),
            2 => Ok(PrgRomSize::Size32KB),
            _ => bail!("Unsupported PRG ROM size"),
        }
    }

    pub fn mirror_mask(&self) -> u16 {
        match self {
            PrgRomSize::Size16KB => 0b0011_1111_1111_1111,
            PrgRomSize::Size32KB => 0b0111_1111_1111_1111,
        }
    }

    pub fn as_size_bytes(&self) -> usize {
        match self {
            PrgRomSize::Size16KB => 16 * 1024,
            PrgRomSize::Size32KB => 32 * 1024,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ChrRomSize {
    size_kb: usize,
}

impl ChrRomSize {
    pub fn parse(raw: &[u8]) -> anyhow::Result<Self> {
        let size_kb = raw[5] as usize * 8;
        Ok(ChrRomSize { size_kb })
    }

    pub fn as_size_bytes(&self) -> usize {
        self.size_kb * 1024
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mapper {
    None,
}

impl Mapper {
    fn parse(raw: &[u8]) -> anyhow::Result<Mapper> {
        let value = (raw[7] & 0b1111_0000) | (raw[6] >> 4);
        match value {
            0 => Ok(Mapper::None),
            _ => bail!("Unsupported mapper: {value}"),
        }
    }
}

pub enum ScreenMirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

impl ScreenMirroring {
    fn parse(raw: &[u8]) -> anyhow::Result<ScreenMirroring> {
        let four_screen = (raw[6] & 0b1000) != 0;
        let vertical = (raw[6] & 0b0001) != 0;
        match (four_screen, vertical) {
            (true, _) => Ok(ScreenMirroring::FourScreen),
            (false, true) => Ok(ScreenMirroring::Vertical),
            (false, false) => Ok(ScreenMirroring::Horizontal),
        }
    }
}

fn check_nes_header(raw: &[u8]) -> anyhow::Result<()> {
    if &raw[0..4] != b"NES\x1A" {
        bail!("Invalid NES ROM header");
    }

    Ok(())
}

fn check_ines_version(raw: &[u8]) -> anyhow::Result<()> {
    let ines_ver = raw[7] & 0b0000_0011;
    if ines_ver != 0 {
        bail!("Unsupported iNES version: {ines_ver}");
    }

    Ok(())
}

fn parse_prg_chr_rom(raw: &[u8]) -> anyhow::Result<(PrgRomSize, Vec<u8>, ChrRomSize, Vec<u8>)> {
    let prg_rom_size = PrgRomSize::parse(raw)?;
    let chr_rom_size = ChrRomSize::parse(raw)?;

    let has_trainer = raw[6] & 0b100 != 0;
    let prg_rom_start = 16 + if has_trainer { 512 } else { 0 };
    let prg_rom_end = prg_rom_start + prg_rom_size.as_size_bytes();
    let chr_rom_start = prg_rom_end;
    let chr_rom_end = chr_rom_start + chr_rom_size.as_size_bytes();
    Ok((
        prg_rom_size,
        raw[prg_rom_start..prg_rom_end].to_vec(),
        chr_rom_size,
        raw[chr_rom_start..chr_rom_end].to_vec(),
    ))
}

impl Peripheral for Rom {
    fn read(&mut self, address: u16) -> u8 {
        // CPU can access PRG ROM only - No need to consider CHR ROM.
        let address = match self.mapper {
            Mapper::None => address & self.prg_rom_size.mirror_mask(),
        };
        self.prg_rom_data[address as usize]
    }

    fn write(&mut self, _address: u16, _value: u8) {
        // ROM is read-only; writes are ignored.
        warn!("Attempted to write to ROM, which is read-only.");
    }
}

#[cfg(test)]
mod tests {
    use crate::hardware::rom::{ChrRomSize, Mapper, PrgRomSize, Rom};

    #[test]
    fn parse_ok() {
        let binary = include_bytes!("../../rom/nestest.nes");
        assert_eq!(binary.len(), 16 + 16384 + 8192);

        let rom = Rom::parse(binary).expect("should parse tester rom correctly");
        assert_eq!(rom.mapper, Mapper::None);

        assert_eq!(rom.prg_rom_size, PrgRomSize::Size16KB);
        assert_eq!(&binary[16..(16 + 16384)], rom.prg_rom_data.as_slice());

        assert_eq!(rom._chr_rom_size, ChrRomSize { size_kb: 8 });
        assert_eq!(
            &binary[(16 + 16384)..(16 + 16384 + 8192)],
            rom._chr_rom_data.as_slice()
        );

        // Check entry point
        assert_eq!(rom.prg_rom_data[0x3ffc], 0x04);
        assert_eq!(rom.prg_rom_data[0x3ffd], 0xc0);
    }
}
