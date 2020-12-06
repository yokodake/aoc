#![allow(dead_code, unused_macros)]
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

const CHUNK: usize = 8;
const LSIZE: usize = 31;
const SLOPES: [(usize, usize); 5] = [(1, 1), (5, 1), (7, 1), (1, 2), (3, 1)];

fn saturate(pred: bool) -> u8 {
    (pred as u8) * u8::MAX
}

#[inline(always)]
unsafe fn get_char(src: &[[u8; 32]], line: usize, rel: usize, slope: usize) -> u8 {
    let (h, v) = SLOPES.get_unchecked(slope);
    *src.get_unchecked(rel)
        .get_unchecked(((line * h) / v) % LSIZE)
        & saturate(line % v == 0)
}

unsafe fn mm256_load8x4_epi8(src: &[[u8; 32]], line: usize) -> __m256i {
    let mut buf: [u8; 32] = [0; 32];
    for i in 0..CHUNK {
        let line = line + i;
        buf[i] = get_char(src, line, i, 0);
        buf[CHUNK + i] = get_char(src, line, i, 1);
        buf[CHUNK * 2 + i] = get_char(src, line, i, 2);
        buf[CHUNK * 3 + i] = get_char(src, line, i, 3);
    }
    let r = _mm256_loadu_si256(buf.as_ptr() as *const __m256i);
    r
}

#[target_feature(enable = "sse2", enable = "avx2")]
unsafe fn driver(mut input: std::fs::File) -> (u32, u32) {
    use std::io::Read;

    let mut bufs = [[0; LSIZE + 1]; CHUNK];
    let mut cnt = _mm_setzero_si128();
    let mut p1 = 0;
    let mut line = 0;

    loop {
        for i in 0..CHUNK {
            match input.read(&mut bufs[i]) {
                Err(_) => return unfuck(&bufs, line, i, p1, cnt),
                Ok(n) => {
                    if n < LSIZE + 1 {
                        return unfuck(&bufs, line, i, p1, cnt);
                    }
                }
            };
        }
        let (p1_, cnt_) = fuck(&bufs, line, cnt);
        p1 += p1_;
        cnt = cnt_;
        line += CHUNK;
    }
}

unsafe fn fuck(bufs: &[[u8; LSIZE + 1]], line: usize, cnt: __m128i) -> (u32, __m128i) {
    let mut p1 = 0;
    for i in 0..CHUNK {
        let line = line + i;
        let ch = get_char(bufs, line, i, 4);
        if ch == b'#' {
            p1 += 1;
        }
    }
    let t = _mm256_cmpeq_epi8(mm256_load8x4_epi8(bufs, line), _mm256_set1_epi8(b'#' as i8));
    let t = _mm256_and_si256(t, _mm256_set1_epi8(1));

    let cn = _mm256_sad_epu8(t, _mm256_setzero_si256());
    let cn = _mm256_permutevar8x32_epi32(cn, _mm256_set_epi32(6, 4, 2, 0, 1, 1, 1, 1));
    let cn = _mm256_extracti128_si256(cn, 1);

    let res = _mm_add_epi32(cnt, cn);
    (p1, res)
}

unsafe fn unfuck(
    bufs: &[[u8; LSIZE + 1]],
    line: usize,
    size: usize,
    p1: u32,
    cnt: __m128i,
) -> (u32, u32) {
    let mut s: [u32; 5] = [0; 5];
    for i in 0..size {
        for j in 0..5 {
            let line = line + i;
            let ch = get_char(bufs, line, i, j);
            if ch == b'#' {
                s[j] += 1;
            }
        }
    }
    let res = _mm_add_epi32(_mm_loadu_si128(s.as_ptr() as *const __m128i), cnt);
    let res = mm_reduce_mul_epu32(res);
    (p1 + s[4], res)
}
unsafe fn mm_reduce_mul_epu32(a: __m128i) -> u32 {
    let m = _mm_mullo_epi32(a, _mm_srli_si128(a, 8));
    let m = _mm_mullo_epi32(m, _mm_srli_si128(m, 4));
    let mf = _mm_cvtsi128_si32(m);
    std::mem::transmute(mf)
}

fn main() {
    let file = std::fs::File::open(std::env::args().nth(1).unwrap());
    match file {
        Err(_) => eprintln!("file fuck"),
        Ok(file) => {
            let (p1, p2) = unsafe { driver(file) };
            println!("p1: {}", p1);
            println!("p2: {}", p1 * p2);
        }
    };
}
