use rand::prelude::IndexedRandom;
use serde::{Deserialize, Serialize};
use strum::{EnumCount, VariantArray};

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[derive(VariantArray, EnumCount)]
pub enum Piece {
    I,
    O,
    T,
    L,
    J,
    S,
    Z,
}

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq, Copy, Clone)]
#[derive(VariantArray, EnumCount)]
pub enum Rotation {
    /// aka 0°
    #[doc(alias = "no-op")]
    North,
    /// aka 90° clockwise
    #[doc(alias = "clockwise")]
    East,
    /// aka 180°
    #[doc(alias = "180")]
    South,
    /// aka 270° clockwise
    #[doc(alias = "counterclockwise")]
    West,
}

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub struct PieceLocation {
    pub piece: Piece,
    pub rotation: Rotation,
    pub spun: bool,
    pub x: i8,
    pub y: i8,
    pub possible_line_clear: bool,
}

#[derive(Debug, Copy, Clone)]
pub struct Board {
    pub cols: [Column; 10],
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Column(pub u64);

impl Column {
    #[inline]
    pub fn height(self) -> u8 {
        64 - self.0.leading_zeros() as u8
    }

    // where `lines.bit(row idx)` indicates whether a particular row's cells are
    // all occupied (i.e. `lines.bit(r) = col0.bit(r) & ... & col9.bit(r)`)
    fn clear(&mut self, mut lines: u64) {
        // while there are rows to remove, iterate
        while lines != 0 {
            // handle the next row (from the bottom) to remove:

            // get the set of (contiguous) rows (starting from the bottom) that
            // are *not* to be removed
            let i = lines.trailing_zeros();
            let mask = (1 << i) - 1; // rows from bottom to keep
            // note: if we're still in this while loop, there must be at least
            // one `1` in `lines` — thus, the next bit left of this mask must be
            // a `1` in `lines` (a row to remove)

            // filter out the bit at `i + 1` (left of the mask, the row directly
            // *above* our contiguous set of rows that aren't complete)
            self.0 = self.0 & mask | self.0 >> 1 & !mask;

            // we handled row `i + 1` — remove it from `lines`
            lines &= !(1 << i);

            // because we removed a row, we need to shift `lines[i+2..]` down so
            // that is stays aligned with `self.0`
            //
            // technically we're trying to just splice out the bit at `i+1`,
            // like we do for `self.0` above; i.e.
            // `lines = lines[0..=i] : lines[i+2..]`
            //
            // but because `line[0..=1]` is guaranteed to be all zeros we can
            // just right shift
            lines >>= 1;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Game {
    pub board: Board,
    pub hold: Piece,
    pub b2b: u64,
    pub b2b_deficit: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct PlacementInfo {
    pub spin: bool,
    pub lines_cleared: u32,
}

type BlockOffset = (i8, i8);
type BlockOffsets = [BlockOffset; 4];


impl Rotation {
    pub const fn rotate_block(&self, (x, y): BlockOffset) -> BlockOffset {
        match self {
            Rotation::North => (x, y),
            Rotation::East => (y, -x),
            Rotation::South => (-x, -y),
            Rotation::West => (-y, x),
        }
    }

    // rotate the offsets for a tetromino
    pub const fn rotate_blocks(&self, blocks: BlockOffsets) -> BlockOffsets {
        [
            self.rotate_block(blocks[0]),
            self.rotate_block(blocks[1]),
            self.rotate_block(blocks[2]),
            self.rotate_block(blocks[3]),
        ]
    }

    pub const fn rotate_right(&self) -> Rotation {
        match self {
            Rotation::North => Rotation::East,
            Rotation::East => Rotation::South,
            Rotation::South => Rotation::West,
            Rotation::West => Rotation::North,
        }
    }

    pub const fn rotate_left(&self) -> Rotation {
        match self {
            Rotation::North => Rotation::West,
            Rotation::West => Rotation::South,
            Rotation::South => Rotation::East,
            Rotation::East => Rotation::North,
        }
    }

    pub const fn rotate_180(&self) -> Rotation {
        match self {
            Rotation::North => Rotation::South,
            Rotation::East => Rotation::West,
            Rotation::South => Rotation::North,
            Rotation::West => Rotation::East,
        }
    }
}

impl Piece {
    // -> (x, y)
    pub const fn blocks(&self) -> BlockOffsets {
        //     +1|⬜️⬜️⬜️
        //      0|⬜️⬜️⬜️
        //     -1|⬜️⬜️⬜️
        // y^ x> |-1 0 1
        match self {
            // +1|🟥🟥⬜️
            //  0|⬜️🟥🟥
            // -1|⬜️⬜️⬜️
            //   |-1 0 1
            Piece::Z => [(-1, 1), (0, 1), (0, 0), (1, 0)],
            // +1|⬜️🟩🟩
            //  0|🟩🟩⬜️
            // -1|⬜️⬜️⬜️
            //   |-1 0 1
            Piece::S => [(-1, 0), (0, 0), (0, 1), (1, 1)],
            // +1|⬜️⬜️⬜️⬜️
            //  0|🟫🟫🟫🟫
            // -1|⬜️⬜️⬜️⬜️
            //   |-1 0 1 2
            Piece::I => [(-1, 0), (0, 0), (1, 0), (2, 0)],
            // +1|⬜️🟨🟨
            //  0|⬜️🟨🟨
            // -1|⬜️⬜️⬜️
            //   |-1 0 1
            Piece::O => [(0, 0), (1, 0), (0, 1), (1, 1)],
            // +1|🟦⬜️⬜️
            //  0|🟦🟦🟦
            // -1|⬜️⬜️⬜️
            //   |-1 0 1
            Piece::J => [(-1, 0), (0, 0), (1, 0), (-1, 1)],
            // +1|⬜️⬜️🟧
            //  0|🟧🟧🟧
            // -1|⬜️⬜️⬜️
            //   |-1 0 1
            Piece::L => [(-1, 0), (0, 0), (1, 0), (1, 1)],
            // +1|⬜️🟪⬜️
            //  0|🟪🟪⬜️
            // -1|⬜️🟪⬜️
            //   |-1 0 1
            Piece::T => [(-1, 0), (0, 0), (1, 0), (0, 1)],
        }
    }
}

macro_rules! lut {
    ($array:expr => $out_ty:ty: $v:pat => $e:expr) => {
        {
            const _LEN: usize = ($array).len();
            let mut out = [const { core::mem::MaybeUninit::uninit() }; _LEN];

            let mut i = 0;
            let arr = $array;
            while i < _LEN {
                let $v = arr[i];
                out[i] = core::mem::MaybeUninit::new($e);

                i += 1;
            }

            unsafe {
                core::mem::transmute::<_, [$out_ty; _LEN]>(out)
            }
        }
    }
}

impl PieceLocation {
    // apply the rotation to the piece, yielding new cell positions
    pub const fn blocks(&self) -> [(i8, i8); 4] {
        // for a given piece, for a given rotation: what are the block offsets?
        const LUT: [[BlockOffsets; Rotation::COUNT]; Piece::COUNT] = lut!(
            Piece::VARIANTS => [BlockOffsets; Rotation::COUNT]: piece => {
                lut!(Rotation::VARIANTS => BlockOffsets: r => {
                    r.rotate_blocks(piece.blocks())
                })
            }
        );

        self.translate_blocks(LUT[self.piece as usize][self.rotation as usize])
    }

    const fn translate(&self, (x, y): BlockOffset) -> (i8, i8) {
        (x + self.x, y + self.y)
    }

    const fn translate_blocks(&self, [a, b, c, d]: BlockOffsets) -> [(i8, i8); 4] {
        [
            self.translate(a),
            self.translate(b),
            self.translate(c),
            self.translate(d),
        ]
    }
}

impl Board {
    pub fn place(&mut self, loc: PieceLocation) -> PlacementInfo {
        for &(x, y) in &loc.blocks() {
            self.cols[x as usize].0 |= 1 << y;
        }
        let line_mask = match loc.possible_line_clear {
            true => self.remove_lines(),
            false => 0,
        };
        PlacementInfo {
            spin: loc.spun,
            lines_cleared: line_mask.count_ones(),
        }
    }

    pub fn remove_lines(&mut self) -> u64 {
        let lines = self.cols.iter().fold(!0, |a, b| a & b.0);
        for c in &mut self.cols {
            c.clear(lines);
        }
        lines
    }
}

impl Game {
    pub fn new(p: Option<Piece>) -> Self {
        let hold = p.unwrap_or_else(|| {
            let mut rng = rand::rng();
            *Piece::VARIANTS.choose(&mut rng).unwrap()
        });

        Self {
            board: Board {
                cols: [Column(0); 10],
            },
            hold,
            b2b: 0,
            b2b_deficit: 0,
        }
    }

    pub fn advance(&mut self, next: Piece, loc: PieceLocation) -> PlacementInfo {
        if loc.piece != next {
            self.hold = next;
        }
        let info = self.board.place(loc);
        if info.lines_cleared > 0 {
            if info.spin || info.lines_cleared == 4 {
                self.b2b += 1;
                self.b2b_deficit = 0;
            } else {
                self.b2b = 0;
            }
        }
        info
    }
}
