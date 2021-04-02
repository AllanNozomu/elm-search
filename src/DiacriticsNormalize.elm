module DiacriticsNormalize exposing (normalizeDiacritics)

import Dict exposing (Dict)

normalizeDiacritics : String -> String
normalizeDiacritics s =
    case String.uncons s of
        Nothing -> ""
        Just (c, sss) -> 
            let
                cc = Maybe.withDefault c <| Dict.get c lookupDict
            in
                String.cons cc (normalizeDiacritics sss)


lookupDict : Dict Char Char
lookupDict =
    Dict.fromList lookupList


lookupList : List ( Char, Char )
lookupList =
    [ ( 'ª', 'a' )
    , ( 'à', 'a' )
    , ( 'á', 'a' )
    , ( 'â', 'a' )
    , ( 'ã', 'a' )
    , ( 'ä', 'a' )
    , ( 'å', 'a' )
    , ( 'ā', 'a' )
    , ( 'ă', 'a' )
    , ( 'ą', 'a' )
    , ( 'ǎ', 'a' )
    , ( 'ȁ', 'a' )
    , ( 'ȃ', 'a' )
    , ( 'ȧ', 'a' )
    , ( 'ᵃ', 'a' )
    , ( 'ḁ', 'a' )
    , ( 'ẚ', 'a' )
    , ( 'ạ', 'a' )
    , ( 'ả', 'a' )
    , ( 'ₐ', 'a' )
    , ( '⒜', 'a' )
    , ( 'ⓐ', 'a' )
    , ( 'ａ', 'a' )
    , ( '𝐚', 'a' )
    , ( '𝑎', 'a' )
    , ( '𝒂', 'a' )
    , ( '𝒶', 'a' )
    , ( '𝓪', 'a' )
    , ( '𝔞', 'a' )
    , ( '𝕒', 'a' )
    , ( '𝖆', 'a' )
    , ( '𝖺', 'a' )
    , ( '𝗮', 'a' )
    , ( '𝘢', 'a' )
    , ( '𝙖', 'a' )
    , ( '𝚊', 'a' )
    , ( 'ᵇ', 'b' )
    , ( 'ḃ', 'b' )
    , ( 'ḅ', 'b' )
    , ( 'ḇ', 'b' )
    , ( '⒝', 'b' )
    , ( 'ⓑ', 'b' )
    , ( 'ｂ', 'b' )
    , ( '𝐛', 'b' )
    , ( '𝑏', 'b' )
    , ( '𝒃', 'b' )
    , ( '𝒷', 'b' )
    , ( '𝓫', 'b' )
    , ( '𝔟', 'b' )
    , ( '𝕓', 'b' )
    , ( '𝖇', 'b' )
    , ( '𝖻', 'b' )
    , ( '𝗯', 'b' )
    , ( '𝘣', 'b' )
    , ( '𝙗', 'b' )
    , ( '𝚋', 'b' )
    , ( 'ç', 'c' )
    , ( 'ć', 'c' )
    , ( 'ĉ', 'c' )
    , ( 'ċ', 'c' )
    , ( 'č', 'c' )
    , ( 'ᶜ', 'c' )
    , ( 'ⅽ', 'c' )
    , ( '⒞', 'c' )
    , ( 'ⓒ', 'c' )
    , ( 'ｃ', 'c' )
    , ( '𝐜', 'c' )
    , ( '𝑐', 'c' )
    , ( '𝒄', 'c' )
    , ( '𝒸', 'c' )
    , ( '𝓬', 'c' )
    , ( '𝔠', 'c' )
    , ( '𝕔', 'c' )
    , ( '𝖈', 'c' )
    , ( '𝖼', 'c' )
    , ( '𝗰', 'c' )
    , ( '𝘤', 'c' )
    , ( '𝙘', 'c' )
    , ( '𝚌', 'c' )
    , ( 'ď', 'd' )
    , ( 'ǆ', 'd' )
    , ( 'ᵈ', 'd' )
    , ( 'ḋ', 'd' )
    , ( 'ḍ', 'd' )
    , ( 'ḏ', 'd' )
    , ( 'ḑ', 'd' )
    , ( 'ḓ', 'd' )
    , ( 'ⅆ', 'd' )
    , ( 'ⅾ', 'd' )
    , ( '⒟', 'd' )
    , ( 'ⓓ', 'd' )
    , ( '㎗', 'd' )
    , ( 'ｄ', 'd' )
    , ( '𝐝', 'd' )
    , ( '𝑑', 'd' )
    , ( '𝒅', 'd' )
    , ( '𝒹', 'd' )
    , ( '𝓭', 'd' )
    , ( '𝔡', 'd' )
    , ( '𝕕', 'd' )
    , ( '𝖉', 'd' )
    , ( '𝖽', 'd' )
    , ( '𝗱', 'd' )
    , ( '𝘥', 'd' )
    , ( '𝙙', 'd' )
    , ( '𝚍', 'd' )
    , ( 'è', 'e' )
    , ( 'é', 'e' )
    , ( 'ê', 'e' )
    , ( 'ë', 'e' )
    , ( 'ē', 'e' )
    , ( 'ĕ', 'e' )
    , ( 'ė', 'e' )
    , ( 'ę', 'e' )
    , ( 'ě', 'e' )
    , ( 'ȅ', 'e' )
    , ( 'ȇ', 'e' )
    , ( 'ȩ', 'e' )
    , ( 'ᵉ', 'e' )
    , ( 'ḙ', 'e' )
    , ( 'ḛ', 'e' )
    , ( 'ẹ', 'e' )
    , ( 'ẻ', 'e' )
    , ( 'ẽ', 'e' )
    , ( 'ₑ', 'e' )
    , ( 'ℯ', 'e' )
    , ( 'ⅇ', 'e' )
    , ( '⒠', 'e' )
    , ( 'ⓔ', 'e' )
    , ( 'ｅ', 'e' )
    , ( '𝐞', 'e' )
    , ( '𝑒', 'e' )
    , ( '𝒆', 'e' )
    , ( '𝓮', 'e' )
    , ( '𝔢', 'e' )
    , ( '𝕖', 'e' )
    , ( '𝖊', 'e' )
    , ( '𝖾', 'e' )
    , ( '𝗲', 'e' )
    , ( '𝘦', 'e' )
    , ( '𝙚', 'e' )
    , ( '𝚎', 'e' )
    , ( 'ᶠ', 'f' )
    , ( 'ḟ', 'f' )
    , ( '⒡', 'f' )
    , ( 'ⓕ', 'f' )
    , ( 'ｆ', 'f' )
    , ( '𝐟', 'f' )
    , ( '𝑓', 'f' )
    , ( '𝒇', 'f' )
    , ( '𝒻', 'f' )
    , ( '𝓯', 'f' )
    , ( '𝔣', 'f' )
    , ( '𝕗', 'f' )
    , ( '𝖋', 'f' )
    , ( '𝖿', 'f' )
    , ( '𝗳', 'f' )
    , ( '𝘧', 'f' )
    , ( '𝙛', 'f' )
    , ( '𝚏', 'f' )
    , ( 'ĝ', 'g' )
    , ( 'ğ', 'g' )
    , ( 'ġ', 'g' )
    , ( 'ģ', 'g' )
    , ( 'ǧ', 'g' )
    , ( 'ǵ', 'g' )
    , ( 'ᵍ', 'g' )
    , ( 'ḡ', 'g' )
    , ( 'ℊ', 'g' )
    , ( '⒢', 'g' )
    , ( 'ⓖ', 'g' )
    , ( '㎍', 'g' )
    , ( 'ｇ', 'g' )
    , ( '𝐠', 'g' )
    , ( '𝑔', 'g' )
    , ( '𝒈', 'g' )
    , ( '𝓰', 'g' )
    , ( '𝔤', 'g' )
    , ( '𝕘', 'g' )
    , ( '𝖌', 'g' )
    , ( '𝗀', 'g' )
    , ( '𝗴', 'g' )
    , ( '𝘨', 'g' )
    , ( '𝙜', 'g' )
    , ( '𝚐', 'g' )
    , ( 'ĥ', 'h' )
    , ( 'ȟ', 'h' )
    , ( 'ʰ', 'h' )
    , ( 'ḣ', 'h' )
    , ( 'ḥ', 'h' )
    , ( 'ḧ', 'h' )
    , ( 'ḩ', 'h' )
    , ( 'ḫ', 'h' )
    , ( 'ẖ', 'h' )
    , ( 'ₕ', 'h' )
    , ( 'ℎ', 'h' )
    , ( '⒣', 'h' )
    , ( 'ⓗ', 'h' )
    , ( 'ｈ', 'h' )
    , ( '𝐡', 'h' )
    , ( '𝒉', 'h' )
    , ( '𝒽', 'h' )
    , ( '𝓱', 'h' )
    , ( '𝔥', 'h' )
    , ( '𝕙', 'h' )
    , ( '𝖍', 'h' )
    , ( '𝗁', 'h' )
    , ( '𝗵', 'h' )
    , ( '𝘩', 'h' )
    , ( '𝙝', 'h' )
    , ( '𝚑', 'h' )
    , ( 'ì', 'i' )
    , ( 'í', 'i' )
    , ( 'î', 'i' )
    , ( 'ï', 'i' )
    , ( 'ĩ', 'i' )
    , ( 'ī', 'i' )
    , ( 'ĭ', 'i' )
    , ( 'į', 'i' )
    , ( 'ǐ', 'i' )
    , ( 'ȉ', 'i' )
    , ( 'ȋ', 'i' )
    , ( 'ᵢ', 'i' )
    , ( 'ḭ', 'i' )
    , ( 'ỉ', 'i' )
    , ( 'ị', 'i' )
    , ( 'ⁱ', 'i' )
    , ( 'ℹ', 'i' )
    , ( 'ⅈ', 'i' )
    , ( 'ⅰ', 'i' )
    , ( '⒤', 'i' )
    , ( 'ⓘ', 'i' )
    , ( 'ｉ', 'i' )
    , ( '𝐢', 'i' )
    , ( '𝑖', 'i' )
    , ( '𝒊', 'i' )
    , ( '𝒾', 'i' )
    , ( '𝓲', 'i' )
    , ( '𝔦', 'i' )
    , ( '𝕚', 'i' )
    , ( '𝖎', 'i' )
    , ( '𝗂', 'i' )
    , ( '𝗶', 'i' )
    , ( '𝘪', 'i' )
    , ( '𝙞', 'i' )
    , ( '𝚒', 'i' )
    , ( 'ĵ', 'j' )
    , ( 'ǰ', 'j' )
    , ( 'ʲ', 'j' )
    , ( 'ⅉ', 'j' )
    , ( '⒥', 'j' )
    , ( 'ⓙ', 'j' )
    , ( 'ⱼ', 'j' )
    , ( 'ｊ', 'j' )
    , ( '𝐣', 'j' )
    , ( '𝑗', 'j' )
    , ( '𝒋', 'j' )
    , ( '𝒿', 'j' )
    , ( '𝓳', 'j' )
    , ( '𝔧', 'j' )
    , ( '𝕛', 'j' )
    , ( '𝖏', 'j' )
    , ( '𝗃', 'j' )
    , ( '𝗷', 'j' )
    , ( '𝘫', 'j' )
    , ( '𝙟', 'j' )
    , ( '𝚓', 'j' )
    , ( 'ķ', 'k' )
    , ( 'ǩ', 'k' )
    , ( 'ᵏ', 'k' )
    , ( 'ḱ', 'k' )
    , ( 'ḳ', 'k' )
    , ( 'ḵ', 'k' )
    , ( 'ₖ', 'k' )
    , ( '⒦', 'k' )
    , ( 'ⓚ', 'k' )
    , ( '㎘', 'k' )
    , ( '㏀', 'k' )
    , ( 'ｋ', 'k' )
    , ( '𝐤', 'k' )
    , ( '𝑘', 'k' )
    , ( '𝒌', 'k' )
    , ( '𝓀', 'k' )
    , ( '𝓴', 'k' )
    , ( '𝔨', 'k' )
    , ( '𝕜', 'k' )
    , ( '𝖐', 'k' )
    , ( '𝗄', 'k' )
    , ( '𝗸', 'k' )
    , ( '𝘬', 'k' )
    , ( '𝙠', 'k' )
    , ( '𝚔', 'k' )
    , ( 'ĺ', 'l' )
    , ( 'ļ', 'l' )
    , ( 'ľ', 'l' )
    , ( 'ŀ', 'l' )
    , ( 'ˡ', 'l' )
    , ( 'ḷ', 'l' )
    , ( 'ḻ', 'l' )
    , ( 'ḽ', 'l' )
    , ( 'ₗ', 'l' )
    , ( 'ℓ', 'l' )
    , ( 'ⅼ', 'l' )
    , ( '⒧', 'l' )
    , ( 'ⓛ', 'l' )
    , ( 'ｌ', 'l' )
    , ( '𝐥', 'l' )
    , ( '𝑙', 'l' )
    , ( '𝒍', 'l' )
    , ( '𝓁', 'l' )
    , ( '𝓵', 'l' )
    , ( '𝔩', 'l' )
    , ( '𝕝', 'l' )
    , ( '𝖑', 'l' )
    , ( '𝗅', 'l' )
    , ( '𝗹', 'l' )
    , ( '𝘭', 'l' )
    , ( '𝙡', 'l' )
    , ( '𝚕', 'l' )
    , ( 'ᵐ', 'm' )
    , ( 'ḿ', 'm' )
    , ( 'ṁ', 'm' )
    , ( 'ṃ', 'm' )
    , ( 'ₘ', 'm' )
    , ( 'ⅿ', 'm' )
    , ( '⒨', 'm' )
    , ( 'ⓜ', 'm' )
    , ( '㎖', 'm' )
    , ( '㎛', 'm' )
    , ( '㎡', 'm' )
    , ( '㎥', 'm' )
    , ( 'ｍ', 'm' )
    , ( '𝐦', 'm' )
    , ( '𝑚', 'm' )
    , ( '𝒎', 'm' )
    , ( '𝓂', 'm' )
    , ( '𝓶', 'm' )
    , ( '𝔪', 'm' )
    , ( '𝕞', 'm' )
    , ( '𝖒', 'm' )
    , ( '𝗆', 'm' )
    , ( '𝗺', 'm' )
    , ( '𝘮', 'm' )
    , ( '𝙢', 'm' )
    , ( '𝚖', 'm' )
    , ( 'ñ', 'n' )
    , ( 'ń', 'n' )
    , ( 'ņ', 'n' )
    , ( 'ň', 'n' )
    , ( 'ŉ', 'n' )
    , ( 'ǹ', 'n' )
    , ( 'ṅ', 'n' )
    , ( 'ṇ', 'n' )
    , ( 'ṉ', 'n' )
    , ( 'ṋ', 'n' )
    , ( 'ⁿ', 'n' )
    , ( 'ₙ', 'n' )
    , ( '⒩', 'n' )
    , ( 'ⓝ', 'n' )
    , ( 'ｎ', 'n' )
    , ( '𝐧', 'n' )
    , ( '𝑛', 'n' )
    , ( '𝒏', 'n' )
    , ( '𝓃', 'n' )
    , ( '𝓷', 'n' )
    , ( '𝔫', 'n' )
    , ( '𝕟', 'n' )
    , ( '𝖓', 'n' )
    , ( '𝗇', 'n' )
    , ( '𝗻', 'n' )
    , ( '𝘯', 'n' )
    , ( '𝙣', 'n' )
    , ( '𝚗', 'n' )
    , ( 'º', 'o' )
    , ( 'ò', 'o' )
    , ( 'ó', 'o' )
    , ( 'ô', 'o' )
    , ( 'õ', 'o' )
    , ( 'ö', 'o' )
    , ( 'ō', 'o' )
    , ( 'ŏ', 'o' )
    , ( 'ő', 'o' )
    , ( 'ơ', 'o' )
    , ( 'ǒ', 'o' )
    , ( 'ǫ', 'o' )
    , ( 'ȍ', 'o' )
    , ( 'ȏ', 'o' )
    , ( 'ȯ', 'o' )
    , ( 'ᵒ', 'o' )
    , ( 'ọ', 'o' )
    , ( 'ỏ', 'o' )
    , ( 'ₒ', 'o' )
    , ( 'ℴ', 'o' )
    , ( '⒪', 'o' )
    , ( 'ⓞ', 'o' )
    , ( 'ｏ', 'o' )
    , ( '𝐨', 'o' )
    , ( '𝑜', 'o' )
    , ( '𝒐', 'o' )
    , ( '𝓸', 'o' )
    , ( '𝔬', 'o' )
    , ( '𝕠', 'o' )
    , ( '𝖔', 'o' )
    , ( '𝗈', 'o' )
    , ( '𝗼', 'o' )
    , ( '𝘰', 'o' )
    , ( '𝙤', 'o' )
    , ( '𝚘', 'o' )
    , ( 'ᵖ', 'p' )
    , ( 'ṕ', 'p' )
    , ( 'ṗ', 'p' )
    , ( 'ₚ', 'p' )
    , ( '⒫', 'p' )
    , ( 'ⓟ', 'p' )
    , ( 'ｐ', 'p' )
    , ( '𝐩', 'p' )
    , ( '𝑝', 'p' )
    , ( '𝒑', 'p' )
    , ( '𝓅', 'p' )
    , ( '𝓹', 'p' )
    , ( '𝔭', 'p' )
    , ( '𝕡', 'p' )
    , ( '𝖕', 'p' )
    , ( '𝗉', 'p' )
    , ( '𝗽', 'p' )
    , ( '𝘱', 'p' )
    , ( '𝙥', 'p' )
    , ( '𝚙', 'p' )
    , ( '⒬', 'q' )
    , ( 'ⓠ', 'q' )
    , ( 'ｑ', 'q' )
    , ( '𝐪', 'q' )
    , ( '𝑞', 'q' )
    , ( '𝒒', 'q' )
    , ( '𝓆', 'q' )
    , ( '𝓺', 'q' )
    , ( '𝔮', 'q' )
    , ( '𝕢', 'q' )
    , ( '𝖖', 'q' )
    , ( '𝗊', 'q' )
    , ( '𝗾', 'q' )
    , ( '𝘲', 'q' )
    , ( '𝙦', 'q' )
    , ( '𝚚', 'q' )
    , ( 'ŕ', 'r' )
    , ( 'ŗ', 'r' )
    , ( 'ř', 'r' )
    , ( 'ȑ', 'r' )
    , ( 'ȓ', 'r' )
    , ( 'ʳ', 'r' )
    , ( 'ᵣ', 'r' )
    , ( 'ṙ', 'r' )
    , ( 'ṛ', 'r' )
    , ( 'ṟ', 'r' )
    , ( '⒭', 'r' )
    , ( 'ⓡ', 'r' )
    , ( 'ｒ', 'r' )
    , ( '𝐫', 'r' )
    , ( '𝑟', 'r' )
    , ( '𝒓', 'r' )
    , ( '𝓇', 'r' )
    , ( '𝓻', 'r' )
    , ( '𝔯', 'r' )
    , ( '𝕣', 'r' )
    , ( '𝖗', 'r' )
    , ( '𝗋', 'r' )
    , ( '𝗿', 'r' )
    , ( '𝘳', 'r' )
    , ( '𝙧', 'r' )
    , ( '𝚛', 'r' )
    , ( 'ś', 's' )
    , ( 'ŝ', 's' )
    , ( 'ş', 's' )
    , ( 'š', 's' )
    , ( 'ſ', 's' )
    , ( 'ș', 's' )
    , ( 'ˢ', 's' )
    , ( 'ṡ', 's' )
    , ( 'ṣ', 's' )
    , ( 'ₛ', 's' )
    , ( '⒮', 's' )
    , ( 'ⓢ', 's' )
    , ( '㎲', 's' )
    , ( 'ｓ', 's' )
    , ( '𝐬', 's' )
    , ( '𝑠', 's' )
    , ( '𝒔', 's' )
    , ( '𝓈', 's' )
    , ( '𝓼', 's' )
    , ( '𝔰', 's' )
    , ( '𝕤', 's' )
    , ( '𝖘', 's' )
    , ( '𝗌', 's' )
    , ( '𝘀', 's' )
    , ( '𝘴', 's' )
    , ( '𝙨', 's' )
    , ( '𝚜', 's' )
    , ( 'ţ', 't' )
    , ( 'ť', 't' )
    , ( 'ț', 't' )
    , ( 'ᵗ', 't' )
    , ( 'ṫ', 't' )
    , ( 'ṭ', 't' )
    , ( 'ṯ', 't' )
    , ( 'ṱ', 't' )
    , ( 'ẗ', 't' )
    , ( 'ₜ', 't' )
    , ( '⒯', 't' )
    , ( 'ⓣ', 't' )
    , ( 'ﬅ', 't' )
    , ( 'ｔ', 't' )
    , ( '𝐭', 't' )
    , ( '𝑡', 't' )
    , ( '𝒕', 't' )
    , ( '𝓉', 't' )
    , ( '𝓽', 't' )
    , ( '𝔱', 't' )
    , ( '𝕥', 't' )
    , ( '𝖙', 't' )
    , ( '𝗍', 't' )
    , ( '𝘁', 't' )
    , ( '𝘵', 't' )
    , ( '𝙩', 't' )
    , ( '𝚝', 't' )
    , ( 'ù', 'u' )
    , ( 'ú', 'u' )
    , ( 'û', 'u' )
    , ( 'ü', 'u' )
    , ( 'ũ', 'u' )
    , ( 'ū', 'u' )
    , ( 'ŭ', 'u' )
    , ( 'ů', 'u' )
    , ( 'ű', 'u' )
    , ( 'ų', 'u' )
    , ( 'ư', 'u' )
    , ( 'ǔ', 'u' )
    , ( 'ȕ', 'u' )
    , ( 'ȗ', 'u' )
    , ( 'ᵘ', 'u' )
    , ( 'ᵤ', 'u' )
    , ( 'ṳ', 'u' )
    , ( 'ṵ', 'u' )
    , ( 'ṷ', 'u' )
    , ( 'ụ', 'u' )
    , ( 'ủ', 'u' )
    , ( '⒰', 'u' )
    , ( 'ⓤ', 'u' )
    , ( 'ｕ', 'u' )
    , ( '𝐮', 'u' )
    , ( '𝑢', 'u' )
    , ( '𝒖', 'u' )
    , ( '𝓊', 'u' )
    , ( '𝓾', 'u' )
    , ( '𝔲', 'u' )
    , ( '𝕦', 'u' )
    , ( '𝖚', 'u' )
    , ( '𝗎', 'u' )
    , ( '𝘂', 'u' )
    , ( '𝘶', 'u' )
    , ( '𝙪', 'u' )
    , ( '𝚞', 'u' )
    , ( 'ᵛ', 'v' )
    , ( 'ᵥ', 'v' )
    , ( 'ṽ', 'v' )
    , ( 'ṿ', 'v' )
    , ( 'ⅴ', 'v' )
    , ( '⒱', 'v' )
    , ( 'ⓥ', 'v' )
    , ( 'ｖ', 'v' )
    , ( '𝐯', 'v' )
    , ( '𝑣', 'v' )
    , ( '𝒗', 'v' )
    , ( '𝓋', 'v' )
    , ( '𝓿', 'v' )
    , ( '𝔳', 'v' )
    , ( '𝕧', 'v' )
    , ( '𝖛', 'v' )
    , ( '𝗏', 'v' )
    , ( '𝘃', 'v' )
    , ( '𝘷', 'v' )
    , ( '𝙫', 'v' )
    , ( '𝚟', 'v' )
    , ( 'ŵ', 'w' )
    , ( 'ʷ', 'w' )
    , ( 'ẁ', 'w' )
    , ( 'ẃ', 'w' )
    , ( 'ẅ', 'w' )
    , ( 'ẇ', 'w' )
    , ( 'ẉ', 'w' )
    , ( 'ẘ', 'w' )
    , ( '⒲', 'w' )
    , ( 'ⓦ', 'w' )
    , ( 'ｗ', 'w' )
    , ( '𝐰', 'w' )
    , ( '𝑤', 'w' )
    , ( '𝒘', 'w' )
    , ( '𝓌', 'w' )
    , ( '𝔀', 'w' )
    , ( '𝔴', 'w' )
    , ( '𝕨', 'w' )
    , ( '𝖜', 'w' )
    , ( '𝗐', 'w' )
    , ( '𝘄', 'w' )
    , ( '𝘸', 'w' )
    , ( '𝙬', 'w' )
    , ( '𝚠', 'w' )
    , ( 'ˣ', 'x' )
    , ( 'ẋ', 'x' )
    , ( 'ẍ', 'x' )
    , ( 'ₓ', 'x' )
    , ( 'ⅹ', 'x' )
    , ( '⒳', 'x' )
    , ( 'ⓧ', 'x' )
    , ( 'ｘ', 'x' )
    , ( '𝐱', 'x' )
    , ( '𝑥', 'x' )
    , ( '𝒙', 'x' )
    , ( '𝓍', 'x' )
    , ( '𝔁', 'x' )
    , ( '𝔵', 'x' )
    , ( '𝕩', 'x' )
    , ( '𝖝', 'x' )
    , ( '𝗑', 'x' )
    , ( '𝘅', 'x' )
    , ( '𝘹', 'x' )
    , ( '𝙭', 'x' )
    , ( '𝚡', 'x' )
    , ( 'ý', 'y' )
    , ( 'ÿ', 'y' )
    , ( 'ŷ', 'y' )
    , ( 'ȳ', 'y' )
    , ( 'ʸ', 'y' )
    , ( 'ẏ', 'y' )
    , ( 'ẙ', 'y' )
    , ( 'ỳ', 'y' )
    , ( 'ỵ', 'y' )
    , ( 'ỷ', 'y' )
    , ( 'ỹ', 'y' )
    , ( '⒴', 'y' )
    , ( 'ⓨ', 'y' )
    , ( 'ｙ', 'y' )
    , ( '𝐲', 'y' )
    , ( '𝑦', 'y' )
    , ( '𝒚', 'y' )
    , ( '𝓎', 'y' )
    , ( '𝔂', 'y' )
    , ( '𝔶', 'y' )
    , ( '𝕪', 'y' )
    , ( '𝖞', 'y' )
    , ( '𝗒', 'y' )
    , ( '𝘆', 'y' )
    , ( '𝘺', 'y' )
    , ( '𝙮', 'y' )
    , ( '𝚢', 'y' )
    , ( 'ź', 'z' )
    , ( 'ż', 'z' )
    , ( 'ž', 'z' )
    , ( 'ᶻ', 'z' )
    , ( 'ẑ', 'z' )
    , ( 'ẓ', 'z' )
    , ( 'ẕ', 'z' )
    , ( '⒵', 'z' )
    , ( 'ⓩ', 'z' )
    , ( 'ｚ', 'z' )
    , ( '𝐳', 'z' )
    , ( '𝑧', 'z' )
    , ( '𝒛', 'z' )
    , ( '𝓏', 'z' )
    , ( '𝔃', 'z' )
    , ( '𝔷', 'z' )
    , ( '𝕫', 'z' )
    , ( '𝖟', 'z' )
    , ( '𝗓', 'z' )
    , ( '𝘇', 'z' )
    , ( '𝘻', 'z' )
    , ( '𝙯', 'z' )
    , ( '𝚣', 'z' )
    , ( 'À', 'A' )
    , ( 'Á', 'A' )
    , ( 'Â', 'A' )
    , ( 'Ã', 'A' )
    , ( 'Ä', 'A' )
    , ( 'Å', 'A' )
    , ( 'Ā', 'A' )
    , ( 'Ă', 'A' )
    , ( 'Ą', 'A' )
    , ( 'Ǎ', 'A' )
    , ( 'Ȁ', 'A' )
    , ( 'Ȃ', 'A' )
    , ( 'Ȧ', 'A' )
    , ( 'ᴬ', 'A' )
    , ( 'Ḁ', 'A' )
    , ( 'Ạ', 'A' )
    , ( 'Ả', 'A' )
    , ( 'Ⓐ', 'A' )
    , ( '㎂', 'A' )
    , ( 'Ａ', 'A' )
    , ( '𝐀', 'A' )
    , ( '𝐴', 'A' )
    , ( '𝑨', 'A' )
    , ( '𝒜', 'A' )
    , ( '𝓐', 'A' )
    , ( '𝔄', 'A' )
    , ( '𝔸', 'A' )
    , ( '𝕬', 'A' )
    , ( '𝖠', 'A' )
    , ( '𝗔', 'A' )
    , ( '𝘈', 'A' )
    , ( '𝘼', 'A' )
    , ( '𝙰', 'A' )
    , ( '🄐', 'A' )
    , ( '🄰', 'A' )
    , ( 'ᴮ', 'B' )
    , ( 'Ḃ', 'B' )
    , ( 'Ḅ', 'B' )
    , ( 'Ḇ', 'B' )
    , ( 'ℬ', 'B' )
    , ( 'Ⓑ', 'B' )
    , ( 'Ｂ', 'B' )
    , ( '𝐁', 'B' )
    , ( '𝐵', 'B' )
    , ( '𝑩', 'B' )
    , ( '𝓑', 'B' )
    , ( '𝔅', 'B' )
    , ( '𝔹', 'B' )
    , ( '𝕭', 'B' )
    , ( '𝖡', 'B' )
    , ( '𝗕', 'B' )
    , ( '𝘉', 'B' )
    , ( '𝘽', 'B' )
    , ( '𝙱', 'B' )
    , ( '🄑', 'B' )
    , ( '🄱', 'B' )
    , ( 'Ç', 'C' )
    , ( 'Ć', 'C' )
    , ( 'Ĉ', 'C' )
    , ( 'Ċ', 'C' )
    , ( 'Č', 'C' )
    , ( 'ℂ', 'C' )
    , ( '℃', 'C' )
    , ( 'ℭ', 'C' )
    , ( 'Ⅽ', 'C' )
    , ( 'Ⓒ', 'C' )
    , ( 'Ｃ', 'C' )
    , ( '𝐂', 'C' )
    , ( '𝐶', 'C' )
    , ( '𝑪', 'C' )
    , ( '𝒞', 'C' )
    , ( '𝓒', 'C' )
    , ( '𝕮', 'C' )
    , ( '𝖢', 'C' )
    , ( '𝗖', 'C' )
    , ( '𝘊', 'C' )
    , ( '𝘾', 'C' )
    , ( '𝙲', 'C' )
    , ( '🄒', 'C' )
    , ( '🄫', 'C' )
    , ( '🄲', 'C' )
    , ( 'Ď', 'D' )
    , ( 'Ǆ', 'D' )
    , ( 'ǅ', 'D' )
    , ( 'ᴰ', 'D' )
    , ( 'Ḋ', 'D' )
    , ( 'Ḍ', 'D' )
    , ( 'Ḏ', 'D' )
    , ( 'Ḑ', 'D' )
    , ( 'Ḓ', 'D' )
    , ( 'ⅅ', 'D' )
    , ( 'Ⅾ', 'D' )
    , ( 'Ⓓ', 'D' )
    , ( 'Ｄ', 'D' )
    , ( '𝐃', 'D' )
    , ( '𝐷', 'D' )
    , ( '𝑫', 'D' )
    , ( '𝒟', 'D' )
    , ( '𝓓', 'D' )
    , ( '𝔇', 'D' )
    , ( '𝔻', 'D' )
    , ( '𝕯', 'D' )
    , ( '𝖣', 'D' )
    , ( '𝗗', 'D' )
    , ( '𝘋', 'D' )
    , ( '𝘿', 'D' )
    , ( '𝙳', 'D' )
    , ( '🄓', 'D' )
    , ( '🄳', 'D' )
    , ( 'È', 'E' )
    , ( 'É', 'E' )
    , ( 'Ê', 'E' )
    , ( 'Ë', 'E' )
    , ( 'Ē', 'E' )
    , ( 'Ĕ', 'E' )
    , ( 'Ė', 'E' )
    , ( 'Ę', 'E' )
    , ( 'Ě', 'E' )
    , ( 'Ȅ', 'E' )
    , ( 'Ȇ', 'E' )
    , ( 'Ȩ', 'E' )
    , ( 'ᴱ', 'E' )
    , ( 'Ḙ', 'E' )
    , ( 'Ḛ', 'E' )
    , ( 'Ẹ', 'E' )
    , ( 'Ẻ', 'E' )
    , ( 'Ẽ', 'E' )
    , ( 'ℰ', 'E' )
    , ( 'Ⓔ', 'E' )
    , ( 'Ｅ', 'E' )
    , ( '𝐄', 'E' )
    , ( '𝐸', 'E' )
    , ( '𝑬', 'E' )
    , ( '𝓔', 'E' )
    , ( '𝔈', 'E' )
    , ( '𝔼', 'E' )
    , ( '𝕰', 'E' )
    , ( '𝖤', 'E' )
    , ( '𝗘', 'E' )
    , ( '𝘌', 'E' )
    , ( '𝙀', 'E' )
    , ( '𝙴', 'E' )
    , ( '🄔', 'E' )
    , ( '🄴', 'E' )
    , ( 'Ḟ', 'F' )
    , ( '℉', 'F' )
    , ( 'ℱ', 'F' )
    , ( 'Ⓕ', 'F' )
    , ( '㎌', 'F' )
    , ( 'Ｆ', 'F' )
    , ( '𝐅', 'F' )
    , ( '𝐹', 'F' )
    , ( '𝑭', 'F' )
    , ( '𝓕', 'F' )
    , ( '𝔉', 'F' )
    , ( '𝔽', 'F' )
    , ( '𝕱', 'F' )
    , ( '𝖥', 'F' )
    , ( '𝗙', 'F' )
    , ( '𝘍', 'F' )
    , ( '𝙁', 'F' )
    , ( '𝙵', 'F' )
    , ( '🄕', 'F' )
    , ( '🄵', 'F' )
    , ( 'Ĝ', 'G' )
    , ( 'Ğ', 'G' )
    , ( 'Ġ', 'G' )
    , ( 'Ģ', 'G' )
    , ( 'Ǧ', 'G' )
    , ( 'Ǵ', 'G' )
    , ( 'ᴳ', 'G' )
    , ( 'Ḡ', 'G' )
    , ( 'Ⓖ', 'G' )
    , ( 'Ｇ', 'G' )
    , ( '𝐆', 'G' )
    , ( '𝐺', 'G' )
    , ( '𝑮', 'G' )
    , ( '𝒢', 'G' )
    , ( '𝓖', 'G' )
    , ( '𝔊', 'G' )
    , ( '𝔾', 'G' )
    , ( '𝕲', 'G' )
    , ( '𝖦', 'G' )
    , ( '𝗚', 'G' )
    , ( '𝘎', 'G' )
    , ( '𝙂', 'G' )
    , ( '𝙶', 'G' )
    , ( '🄖', 'G' )
    , ( '🄶', 'G' )
    , ( 'Ĥ', 'H' )
    , ( 'Ȟ', 'H' )
    , ( 'ᴴ', 'H' )
    , ( 'Ḣ', 'H' )
    , ( 'Ḥ', 'H' )
    , ( 'Ḧ', 'H' )
    , ( 'Ḩ', 'H' )
    , ( 'Ḫ', 'H' )
    , ( 'ℋ', 'H' )
    , ( 'ℌ', 'H' )
    , ( 'ℍ', 'H' )
    , ( 'Ⓗ', 'H' )
    , ( 'Ｈ', 'H' )
    , ( '𝐇', 'H' )
    , ( '𝐻', 'H' )
    , ( '𝑯', 'H' )
    , ( '𝓗', 'H' )
    , ( '𝕳', 'H' )
    , ( '𝖧', 'H' )
    , ( '𝗛', 'H' )
    , ( '𝘏', 'H' )
    , ( '𝙃', 'H' )
    , ( '𝙷', 'H' )
    , ( '🄗', 'H' )
    , ( '🄷', 'H' )
    , ( 'Ì', 'I' )
    , ( 'Í', 'I' )
    , ( 'Î', 'I' )
    , ( 'Ï', 'I' )
    , ( 'Ĩ', 'I' )
    , ( 'Ī', 'I' )
    , ( 'Ĭ', 'I' )
    , ( 'Į', 'I' )
    , ( 'İ', 'I' )
    , ( 'Ǐ', 'I' )
    , ( 'Ȉ', 'I' )
    , ( 'Ȋ', 'I' )
    , ( 'ᴵ', 'I' )
    , ( 'Ḭ', 'I' )
    , ( 'Ỉ', 'I' )
    , ( 'Ị', 'I' )
    , ( 'ℐ', 'I' )
    , ( 'ℑ', 'I' )
    , ( 'Ⅰ', 'I' )
    , ( 'Ⓘ', 'I' )
    , ( 'Ｉ', 'I' )
    , ( '𝐈', 'I' )
    , ( '𝐼', 'I' )
    , ( '𝑰', 'I' )
    , ( '𝓘', 'I' )
    , ( '𝕀', 'I' )
    , ( '𝕴', 'I' )
    , ( '𝖨', 'I' )
    , ( '𝗜', 'I' )
    , ( '𝘐', 'I' )
    , ( '𝙄', 'I' )
    , ( '𝙸', 'I' )
    , ( '🄘', 'I' )
    , ( '🄸', 'I' )
    , ( 'Ĵ', 'J' )
    , ( 'ᴶ', 'J' )
    , ( 'Ⓙ', 'J' )
    , ( 'Ｊ', 'J' )
    , ( '𝐉', 'J' )
    , ( '𝐽', 'J' )
    , ( '𝑱', 'J' )
    , ( '𝒥', 'J' )
    , ( '𝓙', 'J' )
    , ( '𝔍', 'J' )
    , ( '𝕁', 'J' )
    , ( '𝕵', 'J' )
    , ( '𝖩', 'J' )
    , ( '𝗝', 'J' )
    , ( '𝘑', 'J' )
    , ( '𝙅', 'J' )
    , ( '𝙹', 'J' )
    , ( '🄙', 'J' )
    , ( '🄹', 'J' )
    , ( 'Ķ', 'K' )
    , ( 'Ǩ', 'K' )
    , ( 'ᴷ', 'K' )
    , ( 'Ḱ', 'K' )
    , ( 'Ḳ', 'K' )
    , ( 'Ḵ', 'K' )
    , ( 'Ⓚ', 'K' )
    , ( 'Ｋ', 'K' )
    , ( '𝐊', 'K' )
    , ( '𝐾', 'K' )
    , ( '𝑲', 'K' )
    , ( '𝒦', 'K' )
    , ( '𝓚', 'K' )
    , ( '𝔎', 'K' )
    , ( '𝕂', 'K' )
    , ( '𝕶', 'K' )
    , ( '𝖪', 'K' )
    , ( '𝗞', 'K' )
    , ( '𝘒', 'K' )
    , ( '𝙆', 'K' )
    , ( '𝙺', 'K' )
    , ( '🄚', 'K' )
    , ( '🄺', 'K' )
    , ( 'Ĺ', 'L' )
    , ( 'Ļ', 'L' )
    , ( 'Ľ', 'L' )
    , ( 'Ŀ', 'L' )
    , ( 'ᴸ', 'L' )
    , ( 'Ḷ', 'L' )
    , ( 'Ḻ', 'L' )
    , ( 'Ḽ', 'L' )
    , ( 'ℒ', 'L' )
    , ( 'Ⅼ', 'L' )
    , ( 'Ⓛ', 'L' )
    , ( 'Ｌ', 'L' )
    , ( '𝐋', 'L' )
    , ( '𝐿', 'L' )
    , ( '𝑳', 'L' )
    , ( '𝓛', 'L' )
    , ( '𝔏', 'L' )
    , ( '𝕃', 'L' )
    , ( '𝕷', 'L' )
    , ( '𝖫', 'L' )
    , ( '𝗟', 'L' )
    , ( '𝘓', 'L' )
    , ( '𝙇', 'L' )
    , ( '𝙻', 'L' )
    , ( '🄛', 'L' )
    , ( '🄻', 'L' )
    , ( 'ᴹ', 'M' )
    , ( 'Ḿ', 'M' )
    , ( 'Ṁ', 'M' )
    , ( 'Ṃ', 'M' )
    , ( 'ℳ', 'M' )
    , ( 'Ⅿ', 'M' )
    , ( 'Ⓜ', 'M' )
    , ( '㏁', 'M' )
    , ( 'Ｍ', 'M' )
    , ( '𝐌', 'M' )
    , ( '𝑀', 'M' )
    , ( '𝑴', 'M' )
    , ( '𝓜', 'M' )
    , ( '𝔐', 'M' )
    , ( '𝕄', 'M' )
    , ( '𝕸', 'M' )
    , ( '𝖬', 'M' )
    , ( '𝗠', 'M' )
    , ( '𝘔', 'M' )
    , ( '𝙈', 'M' )
    , ( '𝙼', 'M' )
    , ( '🄜', 'M' )
    , ( '🄼', 'M' )
    , ( 'Ñ', 'N' )
    , ( 'Ń', 'N' )
    , ( 'Ņ', 'N' )
    , ( 'Ň', 'N' )
    , ( 'Ǹ', 'N' )
    , ( 'ᴺ', 'N' )
    , ( 'Ṅ', 'N' )
    , ( 'Ṇ', 'N' )
    , ( 'Ṉ', 'N' )
    , ( 'Ṋ', 'N' )
    , ( 'ℕ', 'N' )
    , ( 'Ⓝ', 'N' )
    , ( 'Ｎ', 'N' )
    , ( '𝐍', 'N' )
    , ( '𝑁', 'N' )
    , ( '𝑵', 'N' )
    , ( '𝒩', 'N' )
    , ( '𝓝', 'N' )
    , ( '𝔑', 'N' )
    , ( '𝕹', 'N' )
    , ( '𝖭', 'N' )
    , ( '𝗡', 'N' )
    , ( '𝘕', 'N' )
    , ( '𝙉', 'N' )
    , ( '𝙽', 'N' )
    , ( '🄝', 'N' )
    , ( '🄽', 'N' )
    , ( 'Ò', 'O' )
    , ( 'Ó', 'O' )
    , ( 'Ô', 'O' )
    , ( 'Õ', 'O' )
    , ( 'Ö', 'O' )
    , ( 'Ō', 'O' )
    , ( 'Ŏ', 'O' )
    , ( 'Ő', 'O' )
    , ( 'Ơ', 'O' )
    , ( 'Ǒ', 'O' )
    , ( 'Ǫ', 'O' )
    , ( 'Ȍ', 'O' )
    , ( 'Ȏ', 'O' )
    , ( 'Ȯ', 'O' )
    , ( 'ᴼ', 'O' )
    , ( 'Ọ', 'O' )
    , ( 'Ỏ', 'O' )
    , ( 'Ⓞ', 'O' )
    , ( 'Ｏ', 'O' )
    , ( '𝐎', 'O' )
    , ( '𝑂', 'O' )
    , ( '𝑶', 'O' )
    , ( '𝒪', 'O' )
    , ( '𝓞', 'O' )
    , ( '𝔒', 'O' )
    , ( '𝕆', 'O' )
    , ( '𝕺', 'O' )
    , ( '𝖮', 'O' )
    , ( '𝗢', 'O' )
    , ( '𝘖', 'O' )
    , ( '𝙊', 'O' )
    , ( '𝙾', 'O' )
    , ( '🄞', 'O' )
    , ( '🄾', 'O' )
    , ( 'ᴾ', 'P' )
    , ( 'Ṕ', 'P' )
    , ( 'Ṗ', 'P' )
    , ( 'ℙ', 'P' )
    , ( 'Ⓟ', 'P' )
    , ( 'Ｐ', 'P' )
    , ( '𝐏', 'P' )
    , ( '𝑃', 'P' )
    , ( '𝑷', 'P' )
    , ( '𝒫', 'P' )
    , ( '𝓟', 'P' )
    , ( '𝔓', 'P' )
    , ( '𝕻', 'P' )
    , ( '𝖯', 'P' )
    , ( '𝗣', 'P' )
    , ( '𝘗', 'P' )
    , ( '𝙋', 'P' )
    , ( '𝙿', 'P' )
    , ( '🄟', 'P' )
    , ( '🄿', 'P' )
    , ( 'ℚ', 'Q' )
    , ( 'Ⓠ', 'Q' )
    , ( 'Ｑ', 'Q' )
    , ( '𝐐', 'Q' )
    , ( '𝑄', 'Q' )
    , ( '𝑸', 'Q' )
    , ( '𝒬', 'Q' )
    , ( '𝓠', 'Q' )
    , ( '𝔔', 'Q' )
    , ( '𝕼', 'Q' )
    , ( '𝖰', 'Q' )
    , ( '𝗤', 'Q' )
    , ( '𝘘', 'Q' )
    , ( '𝙌', 'Q' )
    , ( '𝚀', 'Q' )
    , ( '🄠', 'Q' )
    , ( '🅀', 'Q' )
    , ( 'Ŕ', 'R' )
    , ( 'Ŗ', 'R' )
    , ( 'Ř', 'R' )
    , ( 'Ȑ', 'R' )
    , ( 'Ȓ', 'R' )
    , ( 'ᴿ', 'R' )
    , ( 'Ṙ', 'R' )
    , ( 'Ṛ', 'R' )
    , ( 'Ṟ', 'R' )
    , ( 'ℛ', 'R' )
    , ( 'ℜ', 'R' )
    , ( 'ℝ', 'R' )
    , ( 'Ⓡ', 'R' )
    , ( 'Ｒ', 'R' )
    , ( '𝐑', 'R' )
    , ( '𝑅', 'R' )
    , ( '𝑹', 'R' )
    , ( '𝓡', 'R' )
    , ( '𝕽', 'R' )
    , ( '𝖱', 'R' )
    , ( '𝗥', 'R' )
    , ( '𝘙', 'R' )
    , ( '𝙍', 'R' )
    , ( '𝚁', 'R' )
    , ( '🄡', 'R' )
    , ( '🄬', 'R' )
    , ( '🅁', 'R' )
    , ( 'Ś', 'S' )
    , ( 'Ŝ', 'S' )
    , ( 'Ş', 'S' )
    , ( 'Š', 'S' )
    , ( 'Ș', 'S' )
    , ( 'Ṡ', 'S' )
    , ( 'Ṣ', 'S' )
    , ( 'Ⓢ', 'S' )
    , ( 'Ｓ', 'S' )
    , ( '𝐒', 'S' )
    , ( '𝑆', 'S' )
    , ( '𝑺', 'S' )
    , ( '𝒮', 'S' )
    , ( '𝓢', 'S' )
    , ( '𝔖', 'S' )
    , ( '𝕊', 'S' )
    , ( '𝕾', 'S' )
    , ( '𝖲', 'S' )
    , ( '𝗦', 'S' )
    , ( '𝘚', 'S' )
    , ( '𝙎', 'S' )
    , ( '𝚂', 'S' )
    , ( '🄢', 'S' )
    , ( '🄪', 'S' )
    , ( '🅂', 'S' )
    , ( 'Ţ', 'T' )
    , ( 'Ť', 'T' )
    , ( 'Ț', 'T' )
    , ( 'ᵀ', 'T' )
    , ( 'Ṫ', 'T' )
    , ( 'Ṭ', 'T' )
    , ( 'Ṯ', 'T' )
    , ( 'Ṱ', 'T' )
    , ( 'Ⓣ', 'T' )
    , ( 'Ｔ', 'T' )
    , ( '𝐓', 'T' )
    , ( '𝑇', 'T' )
    , ( '𝑻', 'T' )
    , ( '𝒯', 'T' )
    , ( '𝓣', 'T' )
    , ( '𝔗', 'T' )
    , ( '𝕋', 'T' )
    , ( '𝕿', 'T' )
    , ( '𝖳', 'T' )
    , ( '𝗧', 'T' )
    , ( '𝘛', 'T' )
    , ( '𝙏', 'T' )
    , ( '𝚃', 'T' )
    , ( '🄣', 'T' )
    , ( '🅃', 'T' )
    , ( 'Ù', 'U' )
    , ( 'Ú', 'U' )
    , ( 'Û', 'U' )
    , ( 'Ü', 'U' )
    , ( 'Ũ', 'U' )
    , ( 'Ū', 'U' )
    , ( 'Ŭ', 'U' )
    , ( 'Ů', 'U' )
    , ( 'Ű', 'U' )
    , ( 'Ų', 'U' )
    , ( 'Ư', 'U' )
    , ( 'Ǔ', 'U' )
    , ( 'Ȕ', 'U' )
    , ( 'Ȗ', 'U' )
    , ( 'ᵁ', 'U' )
    , ( 'Ṳ', 'U' )
    , ( 'Ṵ', 'U' )
    , ( 'Ṷ', 'U' )
    , ( 'Ụ', 'U' )
    , ( 'Ủ', 'U' )
    , ( 'Ⓤ', 'U' )
    , ( 'Ｕ', 'U' )
    , ( '𝐔', 'U' )
    , ( '𝑈', 'U' )
    , ( '𝑼', 'U' )
    , ( '𝒰', 'U' )
    , ( '𝓤', 'U' )
    , ( '𝔘', 'U' )
    , ( '𝕌', 'U' )
    , ( '𝖀', 'U' )
    , ( '𝖴', 'U' )
    , ( '𝗨', 'U' )
    , ( '𝘜', 'U' )
    , ( '𝙐', 'U' )
    , ( '𝚄', 'U' )
    , ( '🄤', 'U' )
    , ( '🅄', 'U' )
    , ( 'Ṽ', 'V' )
    , ( 'Ṿ', 'V' )
    , ( 'Ⅴ', 'V' )
    , ( 'Ⓥ', 'V' )
    , ( 'ⱽ', 'V' )
    , ( '㎶', 'V' )
    , ( 'Ｖ', 'V' )
    , ( '𝐕', 'V' )
    , ( '𝑉', 'V' )
    , ( '𝑽', 'V' )
    , ( '𝒱', 'V' )
    , ( '𝓥', 'V' )
    , ( '𝔙', 'V' )
    , ( '𝕍', 'V' )
    , ( '𝖁', 'V' )
    , ( '𝖵', 'V' )
    , ( '𝗩', 'V' )
    , ( '𝘝', 'V' )
    , ( '𝙑', 'V' )
    , ( '𝚅', 'V' )
    , ( '🄥', 'V' )
    , ( '🅅', 'V' )
    , ( 'Ŵ', 'W' )
    , ( 'ᵂ', 'W' )
    , ( 'Ẁ', 'W' )
    , ( 'Ẃ', 'W' )
    , ( 'Ẅ', 'W' )
    , ( 'Ẇ', 'W' )
    , ( 'Ẉ', 'W' )
    , ( 'Ⓦ', 'W' )
    , ( '㎼', 'W' )
    , ( 'Ｗ', 'W' )
    , ( '𝐖', 'W' )
    , ( '𝑊', 'W' )
    , ( '𝑾', 'W' )
    , ( '𝒲', 'W' )
    , ( '𝓦', 'W' )
    , ( '𝔚', 'W' )
    , ( '𝕎', 'W' )
    , ( '𝖂', 'W' )
    , ( '𝖶', 'W' )
    , ( '𝗪', 'W' )
    , ( '𝘞', 'W' )
    , ( '𝙒', 'W' )
    , ( '𝚆', 'W' )
    , ( '🄦', 'W' )
    , ( '🅆', 'W' )
    , ( 'Ẋ', 'X' )
    , ( 'Ẍ', 'X' )
    , ( 'Ⅹ', 'X' )
    , ( 'Ⓧ', 'X' )
    , ( 'Ｘ', 'X' )
    , ( '𝐗', 'X' )
    , ( '𝑋', 'X' )
    , ( '𝑿', 'X' )
    , ( '𝒳', 'X' )
    , ( '𝓧', 'X' )
    , ( '𝔛', 'X' )
    , ( '𝕏', 'X' )
    , ( '𝖃', 'X' )
    , ( '𝖷', 'X' )
    , ( '𝗫', 'X' )
    , ( '𝘟', 'X' )
    , ( '𝙓', 'X' )
    , ( '𝚇', 'X' )
    , ( '🄧', 'X' )
    , ( '🅇', 'X' )
    , ( 'Ý', 'Y' )
    , ( 'Ŷ', 'Y' )
    , ( 'Ÿ', 'Y' )
    , ( 'Ȳ', 'Y' )
    , ( 'Ẏ', 'Y' )
    , ( 'Ỳ', 'Y' )
    , ( 'Ỵ', 'Y' )
    , ( 'Ỷ', 'Y' )
    , ( 'Ỹ', 'Y' )
    , ( 'Ⓨ', 'Y' )
    , ( 'Ｙ', 'Y' )
    , ( '𝐘', 'Y' )
    , ( '𝑌', 'Y' )
    , ( '𝒀', 'Y' )
    , ( '𝒴', 'Y' )
    , ( '𝓨', 'Y' )
    , ( '𝔜', 'Y' )
    , ( '𝕐', 'Y' )
    , ( '𝖄', 'Y' )
    , ( '𝖸', 'Y' )
    , ( '𝗬', 'Y' )
    , ( '𝘠', 'Y' )
    , ( '𝙔', 'Y' )
    , ( '𝚈', 'Y' )
    , ( '🄨', 'Y' )
    , ( '🅈', 'Y' )
    , ( 'Ź', 'Z' )
    , ( 'Ż', 'Z' )
    , ( 'Ž', 'Z' )
    , ( 'Ẑ', 'Z' )
    , ( 'Ẓ', 'Z' )
    , ( 'Ẕ', 'Z' )
    , ( 'ℤ', 'Z' )
    , ( 'ℨ', 'Z' )
    , ( 'Ⓩ', 'Z' )
    , ( 'Ｚ', 'Z' )
    , ( '𝐙', 'Z' )
    , ( '𝑍', 'Z' )
    , ( '𝒁', 'Z' )
    , ( '𝒵', 'Z' )
    , ( '𝓩', 'Z' )
    , ( '𝖅', 'Z' )
    , ( '𝖹', 'Z' )
    , ( '𝗭', 'Z' )
    , ( '𝘡', 'Z' )
    , ( '𝙕', 'Z' )
    , ( '𝚉', 'Z' )
    , ( '🄩', 'Z' )
    , ( '🅉', 'Z' )
    ]
