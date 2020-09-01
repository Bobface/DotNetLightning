namespace DotNetLightning.Payment.LSAT

open Macaroons
open System.Runtime.CompilerServices

open ResultUtils

[<Extension;AbstractClass;Sealed>]
type CaveatsExtensions() =
    
    /// 'Value' is right hand of '=' in caveats, if it does not contain '=', it will return Error
    [<Extension>]
    static member TryGetValue(caveat: Caveat) =
        let s = caveat.ToString().Split('=')
        if (s.Length <> 2) then CustomResult.Error(sprintf "invalid caveat for lsat %s" (caveat.ToString())) else
        CustomResult.Ok(s.[1].Trim())
        
    /// 'Condition' is left hand of '=' in caveats, if it does not contain '=', it will return Error
    [<Extension>]
    static member TryGetCondition(caveat: Caveat) =
        let s = caveat.ToString().Split('=')
        if (s.Length <> 2) then CustomResult.Error(sprintf "invalid caveat for lsat %s" (caveat.ToString())) else
        CustomResult.Ok(s.[0].Trim())

