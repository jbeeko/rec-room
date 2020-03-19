namespace RecRoom


open System.Text.RegularExpressions

open SerializationSupport

module ConstrainedString =

    [<Newtonsoft.Json.JsonConverter(typeof<ConstrainedStringConverter>)>]
    type IConstrainedString =
        abstract Value : string

    and private ConstrainedStringConverter() =
        inherit Newtonsoft.Json.JsonConverter()

        override _x.CanConvert(t:System.Type) = true

        override _x.WriteJson(writer,value, serializer) =
            let v = value :?> IConstrainedString
            serializer.Serialize(writer, v.Value)

        override _x.ReadJson(reader, t, _existingValue, serializer) =
            let str = serializer.Deserialize<string> reader
            let path = reader.Path
            fromString str t path

        override _x.CanRead = true
        override _x.CanWrite = true

    let create canonicalize isValid ctor (s:string) =
        if isNull s
        then None
        else
            let s' = canonicalize s
            if isValid s'
            then Some (ctor s')
            else None

    let apply f (s:IConstrainedString) =
        s.Value |> f

    let value s = apply id s

    let equals left right =
        (value left) = (value right)

    let compareTo left right =
        (value left).CompareTo (value right)

    let singleLineTrimmed s =
        Regex.Replace(s,"\s"," ").Trim()

    let removeWhitespace s =
        Regex.Replace(s, "\s", "").Trim()

    // Validators

    let lengthValidator len (s:string) =
        s.Length <= len

    let lengthBetweenValidator lenMin lenMax (s:string) =
        lenMin <= s.Length && s.Length <= lenMax

    let numericValidator =
        String.forall (fun c -> System.Char.IsNumber(c))

    let alphaNumericValidator =
        String.forall (fun c -> System.Char.IsLetter(c) || System.Char.IsNumber(c))
