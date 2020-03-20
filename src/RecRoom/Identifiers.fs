namespace RecRoom

open System
open System.Security.Cryptography
open System.Numerics
open System.Text
open Microsoft.FSharp.Reflection

module Identifiers =

    [<Newtonsoft.Json.JsonConverter(typeof<IdConverter>)>]
    type IId = interface end
    and private IdConverter() =
      inherit Newtonsoft.Json.JsonConverter()

        override _x.CanConvert(t:System.Type) = true

        override x.WriteJson(writer,value, serializer) =
            let _info, vals = FSharpValue.GetUnionFields (value,null)
            serializer.Serialize(writer, Seq.exactlyOne vals)

        override _x.ReadJson(reader, t, _existingValue, serializer) =
            let str = serializer.Deserialize<string> reader :> obj
            let _info = FSharpType.GetUnionCases (t) |> Seq.exactlyOne
            let info = (FSharpType.GetUnionCases (t)).[0]
            FSharpValue.MakeUnion(info, [|str|])

        override _x.CanRead = true
        override _x.CanWrite = true

    let rec printBase62 (i:BigInteger) =
        let alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let b = BigInteger alphabet.Length
        if i < b
        then alphabet.[int i].ToString()
        else
            let i',  r = BigInteger.DivRem(i, b)
            printBase62(i') + alphabet.[int r].ToString()

    ///**Description** - Creates a random string 8 characters long in base 62. The source
    ///of randomness is 6 bytes of the SHA256 of a new guid.
    let rec newRandom8 () =
        use sha256 = SHA256Managed.Create()
        let bytes = sha256.ComputeHash(Guid.NewGuid().ToByteArray()) |> Array.take 6
        let num = BigInteger(BitConverter.ToUInt64((Array.append bytes [|0uy; 0uy|]), 0))
        // 62^6 = 218340105584896 base 10 or 100000000 base 62 so one
        // less is max num that can be represented in 8 base 62 chars
        if num < 218340105584895I
        then (printBase62 (num + 1I)).PadLeft(8, '0')
        else newRandom8 ()

    let base62Encode (array : byte [])  =
        // Creates a BigInteger from the bytes and adds it to a mask the is a power of
        // 10 greater than the maximum possible integer that can be created by an of the
        // input size. If the integer is negative it adds twice the mask..

        // This encodes 128 bit values as 22 base62 characters.

        let maxInteger = BigInteger.Pow(256I, array.Length)
        let power = (int (Math.Truncate(BigInteger.Log10(maxInteger)))) + 1
        let asInt = BigInteger(array)
        let mask =
            if asInt < 0I
            then BigInteger.Pow(10I, power) * 2I
            else BigInteger.Pow(10I, power)
        printBase62 (BigInteger.Abs(asInt) + mask)

    let guidId () : string =
        base62Encode (Guid.NewGuid().ToByteArray())

    type IdLength =
        | Len2 = 2
        | Len3 = 3
        | Len4 = 4
        | Len5 = 5
        | Len6 = 6
        | Len12 = 12
        | Max = 0

    let private randString (seed: byte array) (len: IdLength) : string =
        // SHA512 truncated to 16 bytes is more robust than using SHA256
        // because it is a more modern implementation
        use hasher = SHA512Managed.Create()
        let str = (hasher.ComputeHash(seed) |> Array.take 16) |> base62Encode
        match len with
        | IdLength.Max -> str
        | _ ->  str.Substring(0, int len)

    let seededId (str:string) (len: IdLength) : string=
        let bytes = Encoding.UTF8.GetBytes(str)
        randString bytes len

    let unseededId (len: IdLength) : string =
        randString (Guid.NewGuid().ToByteArray())  len

    let idFromString str = seededId str IdLength.Max
