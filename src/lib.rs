use std::{borrow::Cow, marker::PhantomData};

use serde::{
    de::{MapAccess, SeqAccess, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use serde_json::{Map, Value};

pub const GENERAL_ERROR: RpcError = RpcError {
    code: -1,
    message: Cow::Borrowed("General error"),
    data: None,
};
pub const PARSE_ERROR: RpcError = RpcError {
    code: -32700,
    message: Cow::Borrowed("Parse error"),
    data: None,
};
pub const INVALID_REQUEST_ERROR: RpcError = RpcError {
    code: -32600,
    message: Cow::Borrowed("Invalid Request"),
    data: None,
};
pub const METHOD_NOT_FOUND_ERROR: RpcError = RpcError {
    code: -32601,
    message: Cow::Borrowed("Method not found"),
    data: None,
};
pub const INVALID_PARAMS_ERROR: RpcError = RpcError {
    code: -32602,
    message: Cow::Borrowed("Invalid params"),
    data: None,
};
pub const INTERNAL_ERROR: RpcError = RpcError {
    code: -32603,
    message: Cow::Borrowed("Internal error"),
    data: None,
};

fn deserialize_some<'de, D: Deserializer<'de>, T: Deserialize<'de>>(
    deserializer: D,
) -> Result<Option<T>, D::Error> {
    T::deserialize(deserializer).map(Some)
}

pub enum SingleOrBatchRpcRequest<T: RpcMethod = AnyRpcMethod<'static>> {
    Single(RpcRequest<T>),
    Batch(Vec<RpcRequest<T>>),
}
impl<T> Serialize for SingleOrBatchRpcRequest<T>
where
    T: RpcMethod,
    RpcRequest<T>: Serialize,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            SingleOrBatchRpcRequest::Single(s) => s.serialize(serializer),
            SingleOrBatchRpcRequest::Batch(b) => b.serialize(serializer),
        }
    }
}
impl<'de, T> Deserialize<'de> for SingleOrBatchRpcRequest<T>
where
    T: RpcMethod + Deserialize<'de>,
    T::Params: Deserialize<'de>,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct ReqVisitor<T>(PhantomData<T>);
        impl<'de, T> Visitor<'de> for ReqVisitor<T>
        where
            T: RpcMethod + Deserialize<'de>,
            T::Params: Deserialize<'de>,
        {
            type Value = SingleOrBatchRpcRequest<T>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "a single rpc request, or a batch of rpc requests"
                )
            }
            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                let mut res = Vec::with_capacity(seq.size_hint().unwrap_or(16));
                while let Some(elem) = seq.next_element()? {
                    res.push(elem);
                }
                Ok(SingleOrBatchRpcRequest::Batch(res))
            }
            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut id = None;
                let mut method = None;
                let mut params = None;
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "id" => {
                            id = map.next_value()?;
                        }
                        "method" => {
                            method = map.next_value()?;
                        }
                        "params" => {
                            params = map.next_value()?;
                        }
                        _ => {
                            let _: Value = map.next_value()?;
                        }
                    }
                }
                Ok(SingleOrBatchRpcRequest::Single(RpcRequest {
                    id,
                    method: method.ok_or_else(|| serde::de::Error::missing_field("method"))?,
                    params: params.ok_or_else(|| serde::de::Error::missing_field("params"))?,
                }))
            }
        }
        deserializer.deserialize_any(ReqVisitor::<T>(PhantomData))
    }
}

pub trait RpcMethod {
    type Params;
    type Response;
    fn as_str<'a>(&'a self) -> &'a str;
}

pub struct GenericRpcMethod<Method: AsRef<str>, Params = AnyParams, Response = Value> {
    method: Method,
    params: PhantomData<Params>,
    response: PhantomData<Response>,
}
impl<Method: AsRef<str>, Params, Response> std::fmt::Debug
    for GenericRpcMethod<Method, Params, Response>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.method.as_ref())
    }
}
impl<Method: AsRef<str>, Params, Response> GenericRpcMethod<Method, Params, Response> {
    pub fn new(method: Method) -> Self {
        GenericRpcMethod {
            method,
            params: PhantomData,
            response: PhantomData,
        }
    }
}
impl<Method: AsRef<str>, Params, Response> Serialize
    for GenericRpcMethod<Method, Params, Response>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(self.method.as_ref(), serializer)
    }
}
impl<'de, Method: AsRef<str> + Deserialize<'de>, Params, Response> Deserialize<'de>
    for GenericRpcMethod<Method, Params, Response>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(GenericRpcMethod::new(Deserialize::deserialize(
            deserializer,
        )?))
    }
}
impl<Method: AsRef<str>, Params, Response> RpcMethod
    for GenericRpcMethod<Method, Params, Response>
{
    type Params = Params;
    type Response = Response;
    fn as_str<'a>(&'a self) -> &'a str {
        self.method.as_ref()
    }
}

pub type AnyRpcMethod<'a> = GenericRpcMethod<Cow<'a, str>>;

#[derive(Debug)]
pub enum AnyParams {
    Positional(Vec<Value>),
    Named(Map<String, Value>),
}
impl Serialize for AnyParams {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            AnyParams::Positional(s) => s.serialize(serializer),
            AnyParams::Named(b) => b.serialize(serializer),
        }
    }
}
impl<'de> Deserialize<'de> for AnyParams {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct ParamVisitor;
        impl<'de> Visitor<'de> for ParamVisitor {
            type Value = AnyParams;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "an array or object")
            }
            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                let mut res = Vec::with_capacity(seq.size_hint().unwrap_or(16));
                while let Some(elem) = seq.next_element()? {
                    res.push(elem);
                }
                Ok(AnyParams::Positional(res))
            }
            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut res = Map::new();

                while let Some((key, value)) = map.next_entry()? {
                    res.insert(key, value);
                }
                Ok(AnyParams::Named(res))
            }
        }
        deserializer.deserialize_any(ParamVisitor)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Id {
    Null,
    String(String),
    Number(serde_json::Number),
}
impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Id::Null => serializer.serialize_none(),
            Id::String(s) => serializer.serialize_str(s),
            Id::Number(n) => {
                #[cfg(feature = "strict")]
                if !n.is_i64() {
                    return Err(serde::ser::Error::custom(
                        "Numbers SHOULD NOT contain fractional parts",
                    ));
                }
                serde_json::Number::serialize(n, serializer)
            }
        }
    }
}
impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct IdVisitor;
        impl<'de> Visitor<'de> for IdVisitor {
            type Value = Id;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a String, Number, or NULL value")
            }
            fn visit_unit<E: serde::de::Error>(self) -> Result<Self::Value, E> {
                Ok(Id::Null)
            }
            fn visit_none<E: serde::de::Error>(self) -> Result<Self::Value, E> {
                Ok(Id::Null)
            }
            fn visit_string<E: serde::de::Error>(self, v: String) -> Result<Self::Value, E> {
                Ok(Id::String(v))
            }
            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                Ok(Id::String(v.to_string()))
            }
            fn visit_f32<E: serde::de::Error>(self, v: f32) -> Result<Self::Value, E> {
                #[cfg(feature = "strict")]
                if v != v.trunc() {
                    return Err(serde::de::Error::custom(
                        "Numbers SHOULD NOT contain fractional parts",
                    ));
                }
                Ok(Id::Number(
                    serde_json::Number::from_f64(v as f64).ok_or_else(|| {
                        serde::de::Error::custom("Infinite or NaN values are not JSON numbers")
                    })?,
                ))
            }
            fn visit_f64<E: serde::de::Error>(self, v: f64) -> Result<Self::Value, E> {
                #[cfg(feature = "strict")]
                if v != v.trunc() {
                    return Err(serde::de::Error::custom(
                        "Numbers SHOULD NOT contain fractional parts",
                    ));
                }
                Ok(Id::Number(serde_json::Number::from_f64(v).ok_or_else(
                    || serde::de::Error::custom("Infinite or NaN values are not JSON numbers"),
                )?))
            }
            fn visit_i8<E: serde::de::Error>(self, v: i8) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_i16<E: serde::de::Error>(self, v: i16) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_i32<E: serde::de::Error>(self, v: i32) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_i64<E: serde::de::Error>(self, v: i64) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_u8<E: serde::de::Error>(self, v: u8) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_u16<E: serde::de::Error>(self, v: u16) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_u32<E: serde::de::Error>(self, v: u32) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
            fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<Self::Value, E> {
                Ok(Id::Number(v.into()))
            }
        }
        deserializer.deserialize_any(IdVisitor)
    }
}
impl PartialOrd for Id {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        match (self, other) {
            (Id::Null, Id::Null) => Some(Equal),
            (Id::Null, _) => Some(Less),
            (_, Id::Null) => Some(Greater),
            (Id::String(a), Id::String(b)) => a.partial_cmp(b),
            (Id::String(_), _) => Some(Less),
            (_, Id::String(_)) => Some(Greater),
            (Id::Number(a), Id::Number(b)) => a.as_f64().partial_cmp(&b.as_f64()),
        }
    }
}
impl Ord for Id {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RpcRequest<T: RpcMethod = AnyRpcMethod<'static>> {
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_some")]
    pub id: Option<Id>,
    pub method: T,
    pub params: T::Params,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcError {
    pub code: i32,
    pub message: Cow<'static, str>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_some")]
    pub data: Option<Value>,
}
impl RpcError {
    pub fn into_anyhow(self) -> anyhow::Error {
        anyhow::anyhow!(self)
    }
}
impl std::fmt::Display for RpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "rpc exited with code {}: {}", self.code, self.message)?;
        if let Some(data) = &self.data {
            write!(f, "\n{:#?}", data)
        } else {
            Ok(())
        }
    }
}
impl<E> From<E> for RpcError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        let err = err.into();
        let mut res = if let Some(json_err) = err.downcast_ref::<serde_json::Error>() {
            if json_err.is_syntax() {
                PARSE_ERROR
            } else {
                INVALID_REQUEST_ERROR
            }
        } else {
            GENERAL_ERROR
        };
        res.data = Some(Value::String(format!("{}", err)));
        res
    }
}

#[derive(Debug, Clone)]
pub struct RpcResponse<T: RpcMethod = AnyRpcMethod<'static>> {
    pub id: Option<Id>,
    pub result: Result<T::Response, RpcError>,
}
impl<Method: RpcMethod> From<RpcError> for RpcResponse<Method> {
    fn from(e: RpcError) -> Self {
        RpcResponse {
            id: None,
            result: Err(e),
        }
    }
}
impl<T> RpcResponse<T>
where
    T: RpcMethod,
{
    pub fn from_result<E: Into<RpcError>>(res: Result<T::Response, E>) -> Self {
        RpcResponse {
            id: None,
            result: res.map_err(|e| e.into()),
        }
    }
}
impl<T> Serialize for RpcResponse<T>
where
    T: RpcMethod,
    T::Response: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map_ser = serializer.serialize_map(Some(3))?;
        map_ser.serialize_entry("jsonrpc", "2.0")?;
        match &self.result {
            Ok(a) => {
                map_ser.serialize_entry("result", a)?;
            }
            Err(e) => {
                map_ser.serialize_entry("error", e)?;
            }
        }
        map_ser.serialize_entry("id", &self.id)?;
        map_ser.end()
    }
}
impl<'de, T> Deserialize<'de> for RpcResponse<T>
where
    T: RpcMethod,
    T::Response: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ResponseVisitor<T>(PhantomData<T>);
        impl<'de, T> Visitor<'de> for ResponseVisitor<T>
        where
            T: RpcMethod,
            T::Response: Deserialize<'de>,
        {
            type Value = RpcResponse<T>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a String, Number, or NULL value")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut verison = None;
                let mut id = None;
                let mut result = None;
                let mut error = None;
                while let Some(k) = map.next_key::<String>()? {
                    match k.as_str() {
                        "jsonrpc" => {
                            verison = Some(map.next_value::<String>()?);
                        }
                        "id" => {
                            id = Some(map.next_value()?);
                        }
                        "result" => {
                            result = Some(map.next_value()?);
                        }
                        "error" => {
                            error = Some(map.next_value()?);
                        }
                        _ => {
                            let _: Value = map.next_value()?;
                        }
                    }
                }
                match verison {
                    Some(v) if v == "2.0" => (),
                    Some(v) => {
                        return Err(serde::de::Error::invalid_value(
                            serde::de::Unexpected::Str(v.as_str()),
                            &"2.0",
                        ))
                    }
                    None => return Err(serde::de::Error::missing_field("jsonrpc")),
                }
                Ok(RpcResponse {
                    id: id.ok_or_else(|| serde::de::Error::missing_field("id"))?,
                    result: match (result, error) {
                        (None, None) => return Err(serde::de::Error::missing_field("result OR error")),
                        (None, Some(e)) => Err(e),
                        (Some(a), None) => Ok(a),
                        (Some(_), Some(_)) => return Err(serde::de::Error::custom("Either the result member or error member MUST be included, but both members MUST NOT be included.")),
                    }
                })
            }
        }
        deserializer.deserialize_map(ResponseVisitor(PhantomData))
    }
}
