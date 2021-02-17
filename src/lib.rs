use serde::{
    de::{SeqAccess, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use serde_json::Value;

pub const GENERAL_ERROR: i32 = -1;
pub const PARSE_ERROR: i32 = -32700;
pub const INVALID_REQUEST_ERROR: i32 = -32600;
pub const METHOD_NOT_FOUND_ERROR: i32 = -32601;
pub const INVALID_PARAMS_ERROR: i32 = -32602;
pub const INTERNAL_ERROR: i32 = -32603;

pub enum SingleOrBatchRpcRequest<T: RpcMethod = GenericRpcMethod> {
    Single(RpcRequest<T>),
    Batch(Vec<RpcRequest<T>>),
}
impl Serialize for SingleOrBatchRpcRequest {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            SingleOrBatchRpcRequest::Single(s) => s.serialize(serializer),
            SingleOrBatchRpcRequest::Batch(b) => b.serialize(serializer),
        }
    }
}
impl<'de> Deserialize<'de> for SingleOrBatchRpcRequest {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct ReqVisitor;
        impl<'de> Visitor<'de> for ReqVisitor {
            type Value = SingleOrBatchRpcRequest;
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
                while let Some(key) = map.next_key()? {
                    match key {
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
                            let _: serde_json::Value = map.next_value()?;
                        }
                    }
                }
                Ok(SingleOrBatchRpcRequest::Single(RpcRequest {
                    jsonrpc: RpcVersion::V2,
                    id,
                    method: method.ok_or_else(|| serde::de::Error::missing_field("method"))?,
                    params: params.ok_or_else(|| serde::de::Error::missing_field("params"))?,
                }))
            }
        }
        deserializer.deserialize_any(ReqVisitor)
    }
}

pub trait RpcMethod {
    type Params: Serialize + for<'de> Deserialize<'de>;
    type Response: Serialize + for<'de> Deserialize<'de>;
    fn as_str<'a>(&'a self) -> &'a str;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GenericRpcMethod(pub String);
impl RpcMethod for GenericRpcMethod {
    type Params = Vec<Value>;
    type Response = Value;
    fn as_str<'a>(&'a self) -> &'a str {
        self.0.as_str()
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum RpcVersion {
    #[serde(rename = "2.0")]
    V2,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RpcRequest<T: RpcMethod> {
    pub jsonrpc: RpcVersion,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Value>,
    pub method: T,
    pub params: T::Params,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RpcError {
    pub code: i32,
    pub message: String,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
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
        let code = if let Some(json_err) = err.downcast_ref::<serde_json::Error>() {
            if json_err.is_syntax() {
                PARSE_ERROR
            } else {
                INVALID_REQUEST_ERROR
            }
        } else {
            GENERAL_ERROR
        };
        RpcError {
            code,
            message: format!("{}", err),
            data: None,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RpcResponse<T: RpcMethod = GenericRpcMethod> {
    pub jsonrpc: RpcVersion,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Value>,
    pub error: Option<RpcError>,
    pub result: Option<T::Response>,
}
impl From<RpcError> for RpcResponse<GenericRpcMethod> {
    fn from(e: RpcError) -> Self {
        RpcResponse {
            jsonrpc: RpcVersion::V2,
            id: None,
            error: Some(e),
            result: None,
        }
    }
}
impl<T: RpcMethod> RpcResponse<T> {
    pub fn into_result(self) -> Result<T::Response, RpcError> {
        match self.error {
            Some(e) => Err(e),
            None => Ok(self.result)
                .transpose()
                .unwrap_or_else(|| serde_json::from_value(Value::Null).map_err(RpcError::from)),
        }
    }
}
