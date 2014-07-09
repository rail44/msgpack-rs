use std::io::IoResult;
use std::collections::HashMap;
use std::iter;

use {
  MsgPack,
  Nil,
  Boolean,
  Binary,
  String,
  Integer,
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64,
  Float,
  Float32,
  Float64,
  Map,
  Array,
  Extended
};

pub struct Parser<T> {
  rdr: T,
}

impl<T: Reader> Parser<T> {
  pub fn new(rdr: T) -> Parser<T> {
    Parser { rdr: rdr }
  }

  pub fn parse(&mut self) -> IoResult<MsgPack> {
    let msgpack = match self.rdr.read_byte() {
      Ok(b) => {
        match b {
          0x00 .. 0x7f => Integer(Uint8(b as u8)),
          0x80 .. 0x8f => Map(try!(self.read_map_data((b - 0x80) as uint))),
          0x90 .. 0x9f => Array(try!(self.read_array_data((b - 0x90) as uint))),
          0xa0 .. 0xbf => String(try!(self.read_bytes((b - 0xa0) as uint).map(|b| String::from_utf8(b).ok().unwrap()))),
          0xc0 => Nil,

          0xc2 => Boolean(false),
          0xc3 => Boolean(true),

          0xc4 => Binary(try!(self.read_bin8())),
          0xc5 => Binary(try!(self.read_bin16())),
          0xc6 => Binary(try!(self.read_bin32())),

          0xc7 => Extended(try!(self.read_ext8())),
          0xc8 => Extended(try!(self.read_ext16())),
          0xc9 => Extended(try!(self.read_ext32())),

          0xca => Float(Float32(try!(self.rdr.read_be_f32()))),
          0xcb => Float(Float64(try!(self.rdr.read_be_f64()))),

          0xcc => Integer(Uint8(try!(self.rdr.read_u8()))),
          0xcd => Integer(Uint16(try!(self.rdr.read_be_u16()))),
          0xce => Integer(Uint32(try!(self.rdr.read_be_u32()))),
          0xcf => Integer(Uint64(try!(self.rdr.read_be_u64()))),

          0xd0 => Integer(Int8(try!(self.rdr.read_i8()))),
          0xd1 => Integer(Int16(try!(self.rdr.read_be_i16()))),
          0xd2 => Integer(Int32(try!(self.rdr.read_be_i32()))),
          0xd3 => Integer(Int64(try!(self.rdr.read_be_i64()))),

          0xd4 => Extended(try!(self.read_fixext1())),
          0xd5 => Extended(try!(self.read_fixext2())),
          0xd6 => Extended(try!(self.read_fixext4())),
          0xd7 => Extended(try!(self.read_fixext8())),
          0xd8 => Extended(try!(self.read_fixext16())),

          0xd9 => String(try!(self.read_bin8().map(|b| String::from_utf8(b).ok().unwrap()))),
          0xda => String(try!(self.read_bin16().map(|b| String::from_utf8(b).ok().unwrap()))),
          0xdb => String(try!(self.read_bin32().map(|b| String::from_utf8(b).ok().unwrap()))),

          0xdc => Array(try!(self.read_array16())),
          0xdd => Array(try!(self.read_array32())),

          0xde => Map(try!(self.read_map16())),
          0xdf => Map(try!(self.read_map32())),

          0xe0 .. 0xff => Integer(Int8(b as i8)),

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
    Ok(buf.to_owned())
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

  fn read_map_data(&mut self, n: uint) -> IoResult<HashMap<String, MsgPack>> {
    let mut itr = iter::range(0, 2*n).map(|_| self.parse());
    let mut map_itr = iter::range(0, n);
    let mut map = HashMap::new();
    for _ in map_itr {
      let key = try!(itr.next().unwrap());
      let value = try!(itr.next().unwrap());
      match key {
        String(s) => {
          map.insert(s, value);
        }
        _ => {
          warn!("Map is now only supported with String key");
        }
      }
    }
    Ok(map)
  }

  fn read_map16(&mut self) -> IoResult<HashMap<String, MsgPack>> {
    let n = try!(self.rdr.read_be_u16());
    self.read_map_data(n as uint)
  }

  fn read_map32(&mut self) -> IoResult<HashMap<String, MsgPack>> {
    let n = try!(self.rdr.read_be_u32());
    self.read_map_data(n as uint)
  }
}

