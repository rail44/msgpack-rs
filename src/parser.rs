use std::io::{
    IoError,
    IoResult
};
use std::collections::TreeMap;
use std::iter;
use std::string::String as RustString;

use MsgPack;
use MsgPack::{
    Nil,
    Boolean,
    String,
    Integer,
    Float,
    Array,
    Binary,
    Extended,
    Map
};
use IntegerValue::{
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64
};
use FloatValue::{
    Float32,
    Float64
};

macro_rules! to_str(
    ($bytes: expr) => (RustString::from_utf8($bytes).map_err(|_| IoError::from_errno(52, true)))
)

pub struct Parser<T> {
    pub rdr: T,
}

impl<T: Reader> Parser<T> {
    pub fn new(rdr: T) -> Parser<T> {
        Parser { rdr: rdr }
    }

    pub fn parse(&mut self) -> IoResult<MsgPack> {
        let msgpack = match self.rdr.read_byte() {
            Ok(b) => {
                match b {
                    0x00 ... 0x7f => Integer(box Uint8(b as u8)),
                    0x80 ... 0x8f => Map(box try!(self.read_map_data((b - 0x80) as uint))),
                    0x90 ... 0x9f => Array(box try!(self.read_array_data((b - 0x90) as uint))),
                    0xa0 ... 0xbf => String(box try!(to_str!(try!(self.read_bytes((b - 0xa0) as uint))))),
                    0xc0 => Nil,

                    0xc2 => Boolean(box false),
                    0xc3 => Boolean(box true),

                    0xc4 => Binary(box try!(self.read_bin8())),
                    0xc5 => Binary(box try!(self.read_bin16())),
                    0xc6 => Binary(box try!(self.read_bin32())),

                    0xc7 => Extended(box try!(self.read_ext8())),
                    0xc8 => Extended(box try!(self.read_ext16())),
                    0xc9 => Extended(box try!(self.read_ext32())),

                    0xca => Float(box Float32(try!(self.rdr.read_be_f32()))),
                    0xcb => Float(box Float64(try!(self.rdr.read_be_f64()))),

                    0xcc => Integer(box Uint8(try!(self.rdr.read_u8()))),
                    0xcd => Integer(box Uint16(try!(self.rdr.read_be_u16()))),
                    0xce => Integer(box Uint32(try!(self.rdr.read_be_u32()))),
                    0xcf => Integer(box Uint64(try!(self.rdr.read_be_u64()))),

                    0xd0 => Integer(box Int8(try!(self.rdr.read_i8()))),
                    0xd1 => Integer(box Int16(try!(self.rdr.read_be_i16()))),
                    0xd2 => Integer(box Int32(try!(self.rdr.read_be_i32()))),
                    0xd3 => Integer(box Int64(try!(self.rdr.read_be_i64()))),

                    0xd4 => Extended(box try!(self.read_fixext1())),
                    0xd5 => Extended(box try!(self.read_fixext2())),
                    0xd6 => Extended(box try!(self.read_fixext4())),
                    0xd7 => Extended(box try!(self.read_fixext8())),
                    0xd8 => Extended(box try!(self.read_fixext16())),


                    0xd9 => String(box try!(to_str!(try!(self.read_bin8())))),
                    0xda => String(box try!(to_str!(try!(self.read_bin16())))),
                    0xdb => String(box try!(to_str!(try!(self.read_bin32())))),

                    0xdc => Array(box try!(self.read_array16())),
                    0xdd => Array(box try!(self.read_array32())),

                    0xde => Map(box try!(self.read_map16())),
                    0xdf => Map(box try!(self.read_map32())),

                    0xe0 ... 0xff => Integer(box Int8(b as i8)),

                    _ => Nil
                }
            }
            Err(e) => return Err(e) 
        };
        Ok(msgpack)
    }

    fn read_bytes(&mut self, min: uint) -> IoResult<Vec<u8>> {
        let mut buf = Vec::with_capacity(min);
        unsafe { buf.set_len(min); }
        try!(self.rdr.read_at_least(min, buf.as_mut_slice()));
        Ok(buf)
    }

    fn read_ext8(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let min = try!(self.rdr.read_u8());
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(min as uint));
        Ok((type_id, data))
    }

    fn read_ext16(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let min = try!(self.rdr.read_be_u16());
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(min as uint));
        Ok((type_id, data))
    }

    fn read_ext32(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let min = try!(self.rdr.read_be_u32());
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(min as uint));
        Ok((type_id, data))
    }

    fn read_bin8(&mut self) -> IoResult<Vec<u8>> {
        let min = try!(self.rdr.read_u8());
        self.read_bytes(min as uint)
    }

    fn read_bin16(&mut self) -> IoResult<Vec<u8>> {
        let min = try!(self.rdr.read_be_u16());
        self.read_bytes(min as uint)
    }

    fn read_bin32(&mut self) -> IoResult<Vec<u8>> {
        let min = try!(self.rdr.read_be_u32());
        self.read_bytes(min as uint)
    }

    fn read_fixext1(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(1));
        Ok((type_id, data))
    }

    fn read_fixext2(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(2));
        Ok((type_id, data))
    }

    fn read_fixext4(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(4));
        Ok((type_id, data))
    }

    fn read_fixext8(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(8));
        Ok((type_id, data))
    }

    fn read_fixext16(&mut self) -> IoResult<(i8, Vec<u8>)> {
        let type_id = try!(self.rdr.read_i8());
        let data = try!(self.read_bytes(16));
        Ok((type_id, data))
    }

    fn read_array_data(&mut self, n: uint) -> IoResult<Vec<MsgPack>> {
        let range = iter::range(0, n);
        let mut itr = range.map(|_| self.parse());
        let mut vec = Vec::new();
        for result in itr {
            vec.push(try!(result))
        }
        Ok(vec)
    }

    fn read_array16(&mut self) -> IoResult<Vec<MsgPack>> {
        let n = try!(self.rdr.read_be_u16());
        self.read_array_data(n as uint)
    }

    fn read_array32(&mut self) -> IoResult<Vec<MsgPack>> {
        let n = try!(self.rdr.read_be_u32());
        self.read_array_data(n as uint)
    }

    fn read_map_data(&mut self, n: uint) -> IoResult<TreeMap<RustString, MsgPack>> {
        let mut itr = iter::range(0, 2*n).map(|_| self.parse());
        let mut map_itr = iter::range(0, n);
        let mut map = TreeMap::new();
        for _ in map_itr {
            let key = try!(itr.next().unwrap());
            let value = try!(itr.next().unwrap());
            match key {
                String(s) => {
                    map.insert(*s, value);
                }
                _ => {
                    warn!("Map is now only supported with String key");
                }
            }
        }
        Ok(map)
    }

    fn read_map16(&mut self) -> IoResult<TreeMap<RustString, MsgPack>> {
        let n = try!(self.rdr.read_be_u16());
        self.read_map_data(n as uint)
    }

    fn read_map32(&mut self) -> IoResult<TreeMap<RustString, MsgPack>> {
        let n = try!(self.rdr.read_be_u32());
        self.read_map_data(n as uint)
    }
}

pub struct StreamParser<T> {
    parser: Parser<T>
}

impl<T: Reader> Iterator<MsgPack> for StreamParser<T> {
    fn next(&mut self) -> Option<MsgPack> {
        match self.parser.parse() {
            Ok(v) => Some(v),
            Err(_) => None
        }
    }
}

impl<T: Reader> StreamParser<T> {
    pub fn new(rdr: T) -> StreamParser<T> {
        StreamParser { parser: Parser::new(rdr) }
    }
}
