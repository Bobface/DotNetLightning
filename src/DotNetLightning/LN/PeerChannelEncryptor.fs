namespace DotNetLightning.LN
open NBitcoin
open NBitcoin.Crypto
open System.IO
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.Aether
open DotNetLightning.Serialize.Msgs
open System
open System.Diagnostics


[<AutoOpen>]
module PeerChannelEncryptor =
    // Sha256("Noise_XK_secp256k1_ChaChaPoly_SHA256")
    let NOISE_CK = [|0x26uy; 0x40uy; 0xf5uy; 0x2euy; 0xebuy; 0xcduy; 0x9euy; 0x88uy; 0x29uy; 0x58uy; 0x95uy; 0x1cuy; 0x79uy; 0x42uy; 0x50uy; 0xeeuy; 0xdbuy; 0x28uy; 0x00uy; 0x2cuy; 0x05uy; 0xd7uy; 0xdcuy; 0x2euy; 0xa0uy; 0xf1uy; 0x95uy; 0x40uy; 0x60uy; 0x42uy; 0xcauy; 0xf1uy|]

    // Sha256(NOISE_CK || "lightning")
    let NOISE_H = [|0xd1uy; 0xfbuy; 0xf6uy; 0xdeuy; 0xe4uy; 0xf6uy; 0x86uy; 0xf1uy; 0x32uy; 0xfduy; 0x70uy; 0x2cuy; 0x4auy; 0xbfuy; 0x8fuy; 0xbauy; 0x4buy; 0xb4uy; 0x20uy; 0xd8uy; 0x9duy; 0x2auy; 0x04uy; 0x8auy; 0x3cuy; 0x4fuy; 0x4cuy; 0x09uy; 0x2euy; 0x37uy; 0xb6uy; 0x76uy|]

    let chacha20 = NSec.Cryptography.ChaCha20Poly1305.ChaCha20Poly1305
    type SharedSecret = NSec.Cryptography.SharedSecret

    module internal SharedSecret =
        let FromKeyPair(pub: PubKey, priv: Key) =
            // SharedSecret.Import()
            ()
        ()

    type NextNoiseStep =
        | ActOne
        | ActTwo
        | ActThree
        | NoiseComplete

    type NoiseStep =
        | PreActOne
        | PostActone
        | PostActTwo

    type BidirectionalNoiseState = {
        /// The handshake hash. This value is the accumulated hash of all handhsake data that has been sent
        /// and received so far during the handshake process.
        H: uint256
        /// The chaining key. This value is the accumulated hash of all previous ECDH outputs.
        /// At the end of the handshake, it is used for deriving the encryption keys for Lightning Messages.
        CK: uint256
    }
        with
            static member h_ =
                (fun state -> state.H),
                (fun h state -> { state with BidirectionalNoiseState.H = h })
            static member ck_ =
                (fun state -> state.CK),
                (fun ck state -> { state with BidirectionalNoiseState.CK = ck })

    type DirectionalNoisestate =
        | OutBound of OutBound
        | InBound of InBound
        with
            static member outBound_: Prism<DirectionalNoisestate, _> =
                (fun state ->
                    match state with
                    | OutBound o -> Some o
                    | InBound i -> None),
                (fun o state ->
                    match state with
                    | OutBound _ -> OutBound o
                    | _ -> state)

            static member inBound_: Prism<DirectionalNoisestate, _> =
                (fun state ->
                    match state with
                    | OutBound _ -> None
                    | InBound i -> Some i),
                (fun i state ->
                    match state with
                    | InBound _ -> InBound i
                    | _ -> state)

    and OutBound = { IE: Key }
        with
            static member ie_ =
                (fun ob -> ob.IE),
                (fun ie ob -> { ob with OutBound.IE = ie})
    and InBound = {
        IE: PubKey option // Some if state >= PostActOne
        RE: Key option // Some if state >= PostActTwo
        TempK2: uint256 option // Some if state >= PostActTwo
    }
        with
            static member ie_: Prism<InBound, _> =
                (fun ib -> ib.IE),
                (fun ie ib -> { ib with InBound.IE = Some ie})

            static member re_ =
                (fun ib -> ib.RE),
                (fun re ib -> { ib with InBound.RE = Some re })

            static member tempK2_ =
                (fun ib -> ib.TempK2),
                (fun tempk2 ib -> {ib with InBound.TempK2 = Some tempk2})

    type NoiseState =
        | InProgress of InProgressNoiseState
        | Finished of FinishedNoiseState
        with
            static member inprogress_ =
                (fun ns ->
                    match ns with
                    | InProgress i -> Some i
                    | _ -> None),
                (fun ipns ns ->
                    match ns with
                    | InProgress _ -> InProgress ipns
                    | _ -> ns)
            static member finished_ =
                (fun ns ->
                    match ns with
                    | Finished state -> Some state
                    | _ -> None),
                (fun finishedNS ns ->
                    match ns with
                    | Finished _ -> Finished finishedNS
                    | _ -> ns)
    and InProgressNoiseState = {
        State: NoiseStep
        DirectionalState: DirectionalNoisestate
        BidirectionalState: BidirectionalNoiseState
    }
        with
            static member state_ =
                (fun ipns -> ipns.State),
                (fun noiseStep ipns -> { ipns with InProgressNoiseState.State = noiseStep })
            static member directionalState_ =
                (fun ipns -> ipns.DirectionalState),
                (fun ds ipns -> { ipns with InProgressNoiseState.DirectionalState = ds })
            static member biDirectionalState_ =
                (fun ipns -> ipns.BidirectionalState),
                (fun bidirectionalState ipns -> { ipns with InProgressNoiseState.BidirectionalState = bidirectionalState })

    and FinishedNoiseState = {
        SK: uint256
        SN: uint64
        SCK: uint256
        RK: uint256
        RN: uint64
        RCK: uint256
    }
        with
            static member sk_ =
                (fun fns -> fns.SK),
                (fun sk fns -> { fns with FinishedNoiseState.SK = sk })
            static member sn_ =
                (fun fns -> fns.SN),
                (fun sn fns -> { fns with FinishedNoiseState.SN = sn })
            static member sck_ =
                (fun fns -> fns.SCK),
                (fun sck fns -> { fns with FinishedNoiseState.SCK = sck })
            static member rk_ =
                (fun fns -> fns.RK),
                (fun rk fns -> { fns with FinishedNoiseState.SCK = rk })
            static member rn_ =
                (fun fns -> fns.RN),
                (fun rn fns -> { fns with FinishedNoiseState.RN = rn })
            static member rck_ =
                (fun fns -> fns.RCK),
                (fun rck fns -> { fns with FinishedNoiseState.RCK = rck })

    let private getNonce (n: uint64) =
        let nonceBytes = ReadOnlySpan(Array.concat[|Array.zeroCreate 4; BitConverter.GetBytes(n) |])
        NSec.Cryptography.Nonce(nonceBytes, 0)

    let hkdfExtractExpand(salt: byte[], ikm: byte[]) =
        let prk = Hashes.HMACSHA256(salt, ikm)
        let t1 = Hashes.HMACSHA256(prk, [|1uy|])
        let t2 = Hashes.HMACSHA256(prk, Array.append t1 [|2uy|])
        (t1, t2)

    type PeerChannelEncryptor(theirNodeId: NodeId option, noiseState: NoiseState) =
        member this.TheirNodeId = theirNodeId
        member val NoiseState = noiseState with get, set

        static member NewOutbound(theirNodeId: NodeId): PeerChannelEncryptor =
            let hashInput = Array.concat[| NOISE_H; theirNodeId.Value.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))
            PeerChannelEncryptor(
                Some(theirNodeId),
                InProgress {
                        State = PreActOne
                        DirectionalState = OutBound ({ IE = Key() })
                        BidirectionalState = {H = h; CK = uint256(NOISE_CK)}
                    }
                )

        static member NewInbound (ourNodeSecret: Key) =
            let hashInput = Array.concat[|NOISE_H; ourNodeSecret.PubKey.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))

            PeerChannelEncryptor(
                None,
                InProgress {
                        State = PreActOne
                        DirectionalState = InBound { IE = None; RE = None; TempK2 = None}
                        BidirectionalState = {H = h; CK = uint256(NOISE_CK)}
                    }
            )

        static member EncryptWithAd(n: uint64, key: uint256, h: ReadOnlySpan<byte>, plainText: ReadOnlySpan<byte>) =
            let nonce = getNonce n
            let keySpan = ReadOnlySpan(key.ToBytes())
            let blobF = NSec.Cryptography.KeyBlobFormat.RawPrivateKey
            use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
            chacha20.Encrypt(chachaKey, &nonce, h, plainText)

        static member DecryptWithAd(n: uint64, key: uint256, h: ReadOnlySpan<byte>, cypherText: ReadOnlySpan<byte>): Result<byte[], HandleError> =
            let nonce = getNonce n
            let blobF = NSec.Cryptography.KeyBlobFormat.RawPrivateKey
            let keySpan = ReadOnlySpan(key.ToBytes())
            use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
            match chacha20.Decrypt(chachaKey, &nonce, h, cypherText) with
            | true, data -> Ok(data)
            | false, _ -> Result.Error {Error = "Bad Mac"; Action = Some( ErrorAction.DisconnectPeer None )}

        static member HKDF(state: BidirectionalNoiseState, ss: SharedSecret): uint256 =
            let hkdf = NSec.Cryptography.HkdfSha256()
            failwith "Not impl"

        static member OutBoundNoiseAct(state: BidirectionalNoiseState, ourKey: Key, theirKey: PubKey): (byte[] * uint256) =
            let ourPub = ourKey.PubKey
            let h = Hashes.SHA256(Array.concat ([| state.H.ToBytes(); ourPub.ToBytes() |]))
            failwith "needs update"

        static member InBoundNoiseAct(state: BidirectionalNoiseState, act: byte[], ourKey: Key): Result<(PubKey * uint256), HandleError> =
            failwith "Not impl"

        member this.GetActOne(): byte[] =
            match this.NoiseState with
            | InProgress { State = state; DirectionalState = dState; BidirectionalState = bState } ->
                match dState with
                | OutBound { IE = ie }->
                    if state <> PreActOne then
                        failwith "Requested act at wrong step"
                    else
                        let (res, _)= PeerChannelEncryptor.OutBoundNoiseAct(bState, ie, this.TheirNodeId.Value.Value)
                        this.NoiseState <- InProgress({ State = PostActone; DirectionalState = dState; BidirectionalState = bState })
                        res
                | _ -> failwith "Wrong direction for act"
            | _ -> failwith "Cannot get act one after noise handshakae completes"

        member this.ProessActOneWithEphemeralKey(actTwo: byte[], ourNodeSecret: Key): Result<byte[], HandleError> = 
            Debug.Assert(actTwo.Length = 50)

            match this.NoiseState with
            | InProgress { State = state; DirectionalState = dState; BidirectionalState = bState } ->
                match dState with
                | OutBound {IE = ie} ->
                    if state = PostActone then
                        match PeerChannelEncryptor.InBoundNoiseAct(bState, actTwo, ie) with
                        | Ok (re, tempK2) ->
                            let ourNodeId = ourNodeSecret.PubKey
                            let res = PeerChannelEncryptor.EncryptWithAd(
                                            1UL,
                                            tempK2,
                                            ReadOnlySpan(bState.H.ToBytes()),
                                            ReadOnlySpan(ourNodeId.ToBytes())
                                        )
                            let h = Hashes.SHA256(Array.concat[| bState.H.ToBytes(); res |])
                            let newbState = { bState with H = uint256(h) }

                            let ss = SharedSecret.FromKeyPair(re, ourNodeSecret)
                            failwith ""
                        | Result.Error _ -> 
                            failwith "Failed to process InboundNoiseAct"
                    else
                        failwith "Requested act at wrong step"
                | _ -> failwith "Wrong direction for act"
            | _ -> failwith "Cannot get act one after noise handshake completes."


        /// Panics if `msg.Length > 65535` (Out of Noise protocol support)
        member this.EncryptMessage(msg: byte[]) =
            failwith "Not impl"
        
        /// Decrypts a message  length header from the remote peer.
        /// panics if noise handshake has not yet finished or `msg.Length != 18`
        member this.DecryptLengthHeader(msg: byte[]): Result<uint16, HandleError> =
            failwith "Not impl"

        /// Decrypts the given message.
        /// panics if `msg.Length > 65535 + 16`
        member this.DecryptMessage(msg: byte[]): Result<byte[], HandleError> =
            failwith "Not impl"

        member this.GetNoiseStep(): NextNoiseStep =
            match this.NoiseState with
            | NoiseState.InProgress s ->
                match s.State with
                | NoiseStep.PreActOne -> NextNoiseStep.ActOne
                | PostActone -> ActTwo
                | PostActTwo -> ActThree
            | Finished _ -> NoiseComplete

        member this.IsReadyForEncryption(): bool =
            match this.NoiseState with
            | InProgress _ -> false
            | Finished _ -> false