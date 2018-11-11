open Lwt.Infix
open BitmexTypes

let symbol_of_string s =
  match s with
  | "XBTUSD" -> XBTUSD
  | _ -> Other s

let string_of_symbol s =
  match s with
  | XBTUSD -> "XBTUSD"
  | _ -> failwith ("Error: string_of_symbol")

let string_of_order_side odr_side =
  match odr_side with
  | Buy -> "Buy"
  | Sell -> "Sell"

let order_side_of_string s =
  match s with
  | "Sell" -> Sell
  | "Buy" -> Buy
  | _ -> failwith ("Error: order_side_of_string cannot handle " ^ s)

let order_side_option_of_string_option so =
  match so with
  | None -> None
  | Some s ->
    match String.uppercase s with
    | "BUY" -> Some Buy
    | "SELL" -> Some Sell
    | _ -> failwith ("Error: order_side_of_string - Cannot handle " ^ s)

let string_of_order_type ord_type =
  match ord_type with
  | Market -> "Market"
  | Limit -> "Limit"
  | Stop -> "Stop"
  | StopLimit -> "StopLimit"
  | MarketIfTouched -> "MarketIfTouched"
  | LimitIfTouched -> "LimitIfTouched"
  | MarketWithLeftOverAsLimit -> "MarketWithLeftOverAsLimit"
  | Pegged -> "Pegged"

let string_of_time_in_force t =
  match t with
  | Day -> "Day"
  | GoodTillCancel -> "GoodTillCancel"
  | ImmediateOrCancel -> "ImmediateOrCancel"
  | FillOrKill -> "FillOrKill"

let string_of_execution_instruction e =
  match e with
  | ParticipateDoNotInitiate -> "ParticipateDoNotInitiate"
  | AllOrNone -> "AllOrNone"
  | MarkPrice -> "MarkPrice"
  | IndexPrice -> "IndexPrice"
  | LastPrice -> "LastPrice"
  | Close -> "Close"
  | ReduceOnly -> "ReduceOnly"
  | Fixed -> "Fixed"

let string_of_contingency_type c =
  match c with
  | OneCancelsTheOther -> "OneCancelsTheOther"
  | OneTriggersTheOther -> "OneTriggersTheOther"
  | OneUpdatesTheOtherAbsolute -> "OneUpdatesTheOtherAbsolute"
  | OneUpdatesTheOtherProportional -> "OneUpdatesTheOtherProportional"

(* Use string of concat here for speed? *)
(* Is there a way to abstract out all the the match statements? *)
let string_of_new_order (new_odr : new_order) =
  "{" ^
  "\"symbol\":\"" ^ (string_of_symbol new_odr.symbol) ^ "\"" ^
  ",\"side\":\"" ^ (string_of_order_side new_odr.side) ^ "\"" ^
  (
    match new_odr.simple_order_qty with
    | None -> ""
    | Some x -> ",\"simpleOrderQty\":" ^ string_of_float x ^ "0"
  ) ^
  (
    match new_odr.order_qty with
    | None -> ""
    | Some x -> ",\"orderQty\":" ^ string_of_int x
  ) ^
  (
    match new_odr.price with
    | None -> ""
    | Some x -> ",\"price\":" ^ string_of_float x ^ "0"
  ) ^
  (
    match new_odr.display_qty with
    | None -> ""
    | Some x -> ",\"displayQty\":" ^ string_of_float x ^ "0"
  ) ^
  (
    match new_odr.stop_px with
    | None -> ""
    | Some x -> ",\"stopPx\":" ^ string_of_float x ^ "0"
  ) ^
  (
    match new_odr.cl_ord_id with
    | None -> ""
    | Some x -> ",\"clOrdId\":\"" ^ x ^ "\""
  ) ^
  (
    match new_odr.cl_ord_link_id with
    | None -> ""
    | Some x -> ",\"clOrdLinkId\":\"" ^ x ^ "\""
  ) ^
  (
    match new_odr.peg_offset_value with
    | None -> ""
    | Some x -> ",\"pegOffsetValue\":" ^ string_of_float x ^ "0"
  ) ^
  (
    match new_odr.peg_price_type with
    | None -> ""
    | Some x -> ",\"pegPriceType\":\"" ^ x ^ "\""
  ) ^
  (
    match new_odr.ord_type with
    | None -> ""
    | Some x -> ",\"ordType\":\"" ^ string_of_order_type x ^ "\""
  ) ^
  (
    match new_odr.time_in_force with
    | None -> ""
    | Some x -> ",\"timeInForce\":\"" ^ string_of_time_in_force x ^ "\""
  ) ^
  (
    match new_odr.exec_inst with
    | None -> ""
    | Some x -> ",\"execInst\":\"" ^ string_of_execution_instruction x ^ "\""
  ) ^
  (
    match new_odr.contingency_type with
    | None -> ""
    | Some x -> ",\"contingencyType\":\"" ^ string_of_contingency_type x ^ "\""
  ) ^
  (
    match new_odr.text_string with
    | None -> ""
    | Some x -> ",\"textString\":\"" ^ x ^ "\""
  ) ^
  "}"

let string_of_order_return odr_ret =
  "{" ^
  "\"order_id = \"" ^ odr_ret.order_id ^ "\"" ^
  (
    match odr_ret.cl_ord_id with
    | None -> ""
    | Some x -> ",\"cl_ord_id\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.cl_ord_link_id with
    | None -> ""
    | Some x -> ",\"cl_ord_link_id\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.account with
    | None -> ""
    | Some x -> ",\"account\":\"" ^ string_of_float x ^ "\""
  ) ^
  ",\"symbol\":\"" ^ string_of_symbol odr_ret.symbol ^ "\"" ^
  (
    match odr_ret.side with
    | None -> ""
    | Some x -> ",\"side\":\"" ^ string_of_order_side x ^ "\""
  ) ^
  (
    match odr_ret.simple_order_qty with
    | None -> ""
    | Some x -> ",\"simple_order_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.order_qty with
    | None -> ""
    | Some x -> ",\"order_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.price with
    | None -> ""
    | Some x -> ",\"price\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.display_qty with
    | None -> ""
    | Some x -> ",\"display_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.stop_px with
    | None -> ""
    | Some x -> ",\"stop_px\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.peg_offset_value with
    | None -> ""
    | Some x -> ",\"peg_offset_value\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.peg_price_type with
    | None -> ""
    | Some x -> ",\"peg_price_type\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.currency with
    | None -> ""
    | Some x -> ",\"currency\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.settl_currency with
    | None -> ""
    | Some x -> ",\"settl_currency\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.ord_type with
    | None -> ""
    | Some x -> ",\"ord_type\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.time_in_force with
    | None -> ""
    | Some x -> ",\"time_in_force\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.exec_inst with
    | None -> ""
    | Some x -> ",\"exec_inst\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.contingency_type with
    | None -> ""
    | Some x -> ",\"contingency_type\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.ex_destination with
    | None -> ""
    | Some x -> ",\"ex_destination\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.ord_status with
    | None -> ""
    | Some x -> ",\"ord_status\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.triggered with
    | None -> ""
    | Some x -> ",\"triggered\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.working_indicator with
    | None -> ""
    | Some x -> ",\"working_indicator\":\"" ^ string_of_bool x ^ "\""
  ) ^
  (
    match odr_ret.ord_rej_reason with
    | None -> ""
    | Some x -> ",\"ord_rej_reason\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.simple_leaves_qty with
    | None -> ""
    | Some x -> ",\"simple_leaves_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.leaves_qty with
    | None -> ""
    | Some x -> ",\"leaves_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.simple_cum_qty with
    | None -> ""
    | Some x -> ",\"simple_cum_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.cum_qty with
    | None -> ""
    | Some x -> ",\"cum_qty\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.avg_px with
    | None -> ""
    | Some x -> ",\"avg_px\":\"" ^ string_of_float x ^ "\""
  ) ^
  (
    match odr_ret.multi_leg_reporting_type with
    | None -> ""
    | Some x -> ",\"multi_leg_reporting_type\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.text with
    | None -> ""
    | Some x -> ",\"text\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.transact_time with
    | None -> ""
    | Some x -> ",\"transact_time\":\"" ^ x ^ "\""
  ) ^
  (
    match odr_ret.timestamp with
    | None -> ""
    | Some x -> ",\"timestamp\":\"" ^ x ^ "\""
  ) ^
  "}"

module Make (U : AccountInfo) = struct

  (* Check out https://github.com/BitMEX/api-connectors/tree/master/clients/python for API *)
  (* endpoint documentation                                                                *)

  let bitmex_api = "https://www.bitmex.com/api/v1"

  (* Post request *)
  let post_it ?nonce ~body path =
    let post_uri = Uri.of_string (bitmex_api ^ path) in
    let timestamp =
      match nonce with
      | None -> string_of_int @@ int_of_float @@ Unix.gettimeofday () *. 100.0
      | Some n -> n
    in
    let message = "POST/api/v1" ^ path ^ timestamp ^ body in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 U.secret_key in
      crypto_obj#add_string message;
      crypto_obj#result
      |> Hex.of_string
      |> function `Hex x -> x
    in
    let headers =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "api-nonce" timestamp
      |> fun h -> Cohttp.Header.add h "api-key" U.api_key
      |> fun h -> Cohttp.Header.add h "api-signature" signature
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
      |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
    in
    Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) post_uri
    >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
    >|= Yojson.Basic.from_string

  (* Get request - pvt -> is this a private API call*)
  let get_it ?nonce ?(pvt = false) path =
    let get_uri = Uri.of_string (bitmex_api ^ path) in
    let timestamp =
      match nonce with
      | None -> string_of_int @@ int_of_float @@ Unix.gettimeofday () *. 100.0
      | Some n -> n
    in
    let message = "GET/api/v1" ^ path ^ timestamp in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 U.secret_key in
      crypto_obj#add_string message;
      crypto_obj#result
      |> Hex.of_string
      |> function `Hex x -> x
    in
    let private_get_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "api-nonce" timestamp
      |> fun h -> Cohttp.Header.add h "api-key" U.api_key
      |> fun h -> Cohttp.Header.add h "api-signature" signature
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
      |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
    in
    let public_get_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
    in
    let get_header =
      match pvt with
      | true -> private_get_header
      | false -> public_get_header
    in
    lwt result = Cohttp_lwt_unix.Client.get ~headers:get_header get_uri in
    lwt result_string = Cohttp_lwt_body.to_string (snd result) in
    let json_result = Yojson.Basic.from_string result_string in
    Lwt.return json_result

  (* Delete request *)
  let delete_it ?nonce ?(pvt = false) path =
    let delete_uri = Uri.of_string (bitmex_api ^ path) in
    let timestamp =
      match nonce with
      | None -> string_of_int @@ int_of_float @@ Unix.gettimeofday () *. 100.0
      | Some n -> n
    in
    let message = "DELETE/api/v1" ^ path ^ timestamp in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 U.secret_key in
      crypto_obj#add_string message;
      crypto_obj#result
      |> Hex.of_string
      |> function `Hex x -> x
    in
    let delete_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "api-nonce" timestamp
      |> fun h -> Cohttp.Header.add h "api-key" U.api_key
      |> fun h -> Cohttp.Header.add h "api-signature" signature
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
      |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
    in
    Cohttp_lwt_unix.Client.delete ~headers:delete_header delete_uri
    >>= fun (a, b) -> Cohttp_lwt_body.to_string b
    >|= Yojson.Basic.from_string

  (* Put Request *)
  let put_it ?nonce ~body path =
    let put_uri = Uri.of_string (bitmex_api ^ path) in
    let timestamp =
      match nonce with
      | None -> string_of_int @@ int_of_float @@ Unix.gettimeofday () *. 100.0
      | Some n -> n
    in
    let message = "PUT/api/v1" ^ path ^ timestamp ^ body in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 U.secret_key in
      crypto_obj#add_string message;
      crypto_obj#result
      |> Hex.of_string
      |> function `Hex x -> x
    in
    let headers =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "api-nonce" timestamp
      |> fun h -> Cohttp.Header.add h "api-key" U.api_key
      |> fun h -> Cohttp.Header.add h "api-signature" signature
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
      |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
    in
    Cohttp_lwt_unix.Client.put ~headers ~body:(Cohttp_lwt_body.of_string body) put_uri
    >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
    >|= Yojson.Basic.from_string


  (*let string_of_new_order new_order = ()
    "{"
      "\"symbol : symbol;"
      "\"side : order_side option;"
      "\"simple_order_qty : float option;"
      "\"order_qty : int option;"
      "\"price : float option;"
      "\"display_qty : float option;"
      "\"stop_px : float option;"
      "\"cl_ord_id : string option;"
      "\"cl_ord_link_id : string option;"
      "\"peg_offset_value : float option;"
      "\"peg_price_type : string option;"
      "\"ord_type : order_type option;"
      "\"time_in_force : time_in_force option;"
      "\"exec_inst : execution_instruction option;"
      "\"contingency_type : contingency_type option;"
      "\"text_string: string option"
    "}"
   *)

  let order_return_of_json json =
    let open Yojson.Basic.Util in
    let f m =
      match member m json with
      | `Null -> None
      | `String s -> Some s
      | _ -> failwith ("Error: order_return_of_json (f) - cannot parse json " ^ (to_string json))
    in
    let g m =
      match member m json with
      | `Null -> None
      | `Float n -> Some n
      | `Int n -> Some (float_of_int n)
      | _ -> failwith ("Error: order_return_of_json (g) - cannot parse json " ^ (to_string json))
    in
    let h m =
      match member m json with
      | `Null -> None
      | `Bool n -> Some n
      | _ -> failwith ("Error: order_return_of_json (h) - cannot parse json " ^ (to_string json))
    in
    if List.mem "error" (keys json)
    then
      let err = member "error" json in
      OrderReturnError {
        error = {
          message = member "message" err |> to_string;
          name = member "name" err |> to_string
        }
      }
    else
      OrderReturn {
        order_id = member "orderID" json |> to_string;
        cl_ord_id = f "clOrdID";
        cl_ord_link_id = f "clOrdLinkID";
        account = g "account";
        symbol = f "symbol" |> (function | Some s -> symbol_of_string s | None -> Other "None");
        side = f "side" |> order_side_option_of_string_option;
        simple_order_qty = g "simpleOrderQty";
        order_qty = g "orderQty";
        price = g "price";
        display_qty = g "displayQty";
        stop_px = g "stopPx";
        peg_offset_value = g "pegOffsetValue";
        peg_price_type = f "pegPriceType";
        currency = f "currency";
        settl_currency = f "settlCurrency";
        ord_type = f "ordType";
        time_in_force = f "timeInForce";
        exec_inst = f "execInst";
        contingency_type = f "contingencyType";
        ex_destination = f "exDestination";
        ord_status = f "ordStatus";
        triggered = f "triggered";
        working_indicator = h "workingIndicator";
        ord_rej_reason = f "ordRejReason";
        simple_leaves_qty = g "simpleLeavesQty";
        leaves_qty = g "leavesQty";
        simple_cum_qty = g "simpleCumQty";
        cum_qty = g "cumQty";
        avg_px = g "avgPx";
        multi_leg_reporting_type = f "multiLegReportingType";
        text = f "text";
        transact_time = f "transactTime";
        timestamp = f "timestamp"
      }

  let position_return_of_json json =
    let open Yojson.Basic.Util in
    let f m =
      match member m json with
      | `Null -> None
      | `String s -> Some s
      | _ -> failwith (
          "Error: position_return_of_json (f) - cannot parse field " ^ m ^
          " in json " ^ (to_string json)
        )
    in
    let g m =
      match member m json with
      | `Null -> None
      | `Float n -> Some n
      | `Int n -> Some (float_of_int n)
      | _ -> failwith (
          "Error: position_return_of_json (g) - cannot parse field " ^ m ^ " in json "
          ^ (to_string json)
        )
    in
    let h m =
      match member m json with
      | `Null -> None
      | `Bool n -> Some n
      | _ -> failwith (
          "Error: position_return_of_json (h) - cannot parse field " ^ m ^
          " in json " ^ (to_string json)
        )
    in
    PositionReturn {
      account = member "account" json |> to_int;
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      currency = member "currency" json |> to_string;
      underlying = f "underlying";
      quote_currency = f "quoteCurrency";
      commission = g "comission";
      init_margin_req = g "initMarginReq";
      maint_margin_req = g "maintMarginReq";
      risk_limit = g "riskLimit";
      leverage = g "leverage";
      cross_margin = h "crossMargin";
      deleverage_percentile = g "deleveragePercentile";
      rebalanced_pnl = g "rebalancedPnl";
      prev_realised_pnl = g "prevRealisedPnl";
      prev_unrealised_pnl = g "prevUnrealisedPnl";
      prev_close_price = g "prevClosePrice";
      opening_timestamp = f "openingTimestamp";
      opening_qty = g "openingQty";
      opening_cost = g "openingCons";
      opening_comm = g "openingComm";
      open_order_buy_qty = g "openOrderBuyQty";
      open_order_buy_cost = g "openOrderBuyCost";
      open_order_buy_premium = g "openOrderBuyPremium";
      open_order_sell_qty = g "openOrderSellQty";
      open_order_sell_cost = g "openOrderSellCost";
      open_order_sell_premium = g "openOrderSellpremium";
      exec_buy_qty = g "execBuyQty";
      exec_buy_cost = g "execBuyCost";
      exec_sell_qty = g "execSellQty";
      exec_sell_cost = g "execSellCost";
      exec_qty = g "execQty";
      exec_cost = g "execCost";
      exec_comm = g "execComm";
      current_timestamp = f "currentTimestamp";
      current_qty = g "currentQty";
      current_cost = g "currentCost";
      current_comm = g "currentComm";
      realised_cost = g "realisedCost";
      unrealised_cost = g "unrealisedCost";
      gross_open_cost = g "grossOpenCost";
      gross_open_premium = g "grossOpenPremium";
      gross_exec_cost = g "grossExecCost";
      is_open = h "isOpen";
      mark_price = g "markPrice";
      mark_value = g "markValue";
      risk_value = g "riskValue";
      home_notional = g "homeNotional";
      foreign_notional = g "foreignNotional";
      pos_state = f "posState";
      pos_cost = g "posCost";
      pos_cost2 = g "posCost2";
      pos_cross = g "posCross";
      pos_init = g "posInit";
      pos_comm = g "posComm";
      pos_loss = g "posLoss";
      pos_margin = g "posMargin";
      pos_maint = g "posMaint";
      pos_allowance = g "posAllowance";
      taxable_margin = g "taxableMargin";
      init_margin = g "initMargin";
      maint_margin = g "maintMargin";
      session_margin = g "sessionMargin";
      target_excess_margin = g "targetExcessMargin";
      var_margin = g "varMargin";
      realised_gross_pnl = g "realisedGrossPnl";
      realised_tax = g "realisedTax";
      realised_pnl = g "realisedPnl";
      unrealised_gross_pnl = g "unrealisedGrossPnl";
      long_bankrupt = g "longBankrupt";
      short_bankrupt = g "shortBankrupt";
      tax_base = g "taxBase";
      indicative_tax_rate = g "indicativeTaxRate";
      indicative_tax = g "indicativeTax";
      unrealised_tax = g "unrealisedTax";
      unrealised_pnl = g "unrealisedPnl";
      unrealised_pnl_pcnt = g "unrealisedPnlPcnt";
      unrealised_roe_pcnt = g "unrealisedRoePcnt";
      simple_qty = g "simpleQty";
      simple_cost = g "simpleCost";
      simple_value = g "simpleValue";
      simple_pnl = g "simplePnl";
      simple_pnl_pcnt = g "simplePnlPcnt";
      avg_cost_price = g "avgCostprice";
      avg_entry_price = g "avgEntryPrice";
      break_even_price = g "breakEvenPrice";
      margin_call_price = g "marginCallPrice";
      liquidation_price = g "liquidationPrice";
      bankrupt_price = g "bankruptPrice";
      timestamp = f "timestamp";
      last_price = g "lastPrice";
      last_value = g "lastValue"
    }

  (* NOTE: Not all API Endpoints are implemented. Ex. get api keys, post to chat, etc... *)

  (***** PUBLIC GET FUNCTIONS *****)

  let get_announcements () = get_it "/announcement"

  let get_chat  () = get_it "/chat"

  let get_chat_channels () = get_it "/chat/channels"

  let get_chat_connected () = get_it "/chat/connected"

  let get_instrument () = get_it "/instrument"

  let get_instrument_active () = get_it "/instrument/active"

  let get_instrument_active_and_indices () = get_it "/instrument/activeAndIndices"

  let get_instrument_active_intervals () = get_it "/instrument/activeIntervals"

  let get_instrument_indices () = get_it "/instrument/indices"

  let get_leaderboard () = get_it "/leaderboard"

  let get_liquidateion () = get_it "/liquidation"

  let get_orderbook ?depth ~symbol () =
    let d =
      match depth with
      | None -> "25"
      | Some d -> string_of_int d
    in
    get_it ("/orderBook?symbol=" ^ (string_of_symbol symbol) ^ "&depth=" ^ d)

  let get_orderbook_l2 ?depth ~symbol () = (*get_it "/orderBook/L2"*)
    let d =
      match depth with
      | None -> "25"
      | Some d -> string_of_int d
    in
    get_it ("/orderBook/L2?symbol=" ^ (string_of_symbol symbol) ^ "&depth=" ^ d)

  let get_quote () = get_it "/quote"

  (* NOTE: Not all functionality is implemented. Missing: filter, columns, count, start, *)
  (*       reverse, start_time & end_time                                                *)
  let get_quote_bucketed ~bin_size ~symbol () =
    get_it ("/quote/bucketed?binSize=" ^ bin_size ^ "&symbol=" ^ symbol)

  let get_schema () = get_it "/schema"

  let get_schema_websocket_help () = get_it "/schema/websocketHelp"

  let get_stats () = get_it "/stats"

  let get_stats_history () = get_it "/stats/history"

  let get_trade () = get_it "/trade"

  (* NOTE: Not all functionality is implemented. Missing: filter, columns, count, start, *)
  (*       reverse, start_time & end_time                                                *)
  let get_trade_bucketed ~bin_size ~symbol () =
    get_it ("/trade/bucketed?binSize=" ^ bin_size ^ "&symbol=" ^ symbol)

  let get_user_check_referral_code ~code () =
    get_it ("/user/checkReferralCode?referralCode=" ^ code)

  (***** PRIVATE GET FUNCTIONS *****)

  let get_execution () = get_it ~pvt:true "/execution"

  let get_execution_trade_history () = get_it ~pvt:true "/execution/tradeHistory"

  let get_funding () = get_it ~pvt:true  "/funding"

  let get_insurance () = get_it ~pvt:true "/insurance"

  (* NOTE: This returns the list of all orders, including open, closed, etc... *)
  let get_orders () =
    get_it ~pvt:true "/order"
    >|= Yojson.Basic.Util.to_list
    >|= List.map (order_return_of_json)

  let get_open_orders () =
    Utils.logger "start get_open_orders";
    lwt all_orders =
      get_it ~pvt:true "/order?reverse=true"
      >|= Yojson.Basic.Util.to_list
      >|= List.map (order_return_of_json)
    in
    let all_open_orders =
      List.filter (fun x ->
        match x with
        | OrderReturn odr_ret -> (
            match odr_ret.leaves_qty with
            | None -> false
            | Some n -> n > 0.0
          )
        | OrderReturnError err -> false
        ) all_orders
    in
    Utils.logger "end get_open_orders";
    Lwt.return all_open_orders

  let get_position () =
    lwt p = get_it ~pvt:true "/position" in
    match p with
    | `List _ -> (
        let l = Yojson.Basic.Util.to_list p in
        (* TODO: An error message similar to order_return_error can be returned, need to handle *)
        Lwt.return @@ List.map (position_return_of_json) l
      )
    | _ -> failwith ("Error: get_position - unhandled message, possibly an error.")

  let get_settlement () = get_it ~pvt:true "/settlement"

  let get_user () = get_it ~pvt:true "/user"

  let get_user_affiliate_status () = get_it ~pvt:true "/user/affiliateStatus"

  let get_user_commission () = get_it ~pvt:true "/user/commission"

  let get_user_deposit_address () = get_it ~pvt:true "/user/depositAddress"

  let get_user_margin () = get_it ~pvt:true "/user/margin"

  let get_user_wallet () = get_it ~pvt:true "/user/wallet"

  let get_user_wallet_history () = get_it ~pvt:true "/user/walletHistory"

  let get_user_wallet_summary () = get_it ~pvt:true "/user/walletSummary"

  let get_user_min_withdrawal_fee () = get_it ~pvt:true "/user/minWithdrawalFee"

  (***** PRIVATE POST FUNCTIONS *****)

  (* NOTE: Set ~ms:0.0 to cancel this timer *)
  let cancel_all_orders_after ~ms () =
    post_it ~body:("{\"timeout\" : " ^ (string_of_int ms) ^ "}") "/order/cancelAllAfter"

  (* NOTE: To close a position use POST /order wtih execInst:'Close'*)
  let close_position close_order =
    post_it ~body:(string_of_new_order close_order) "/order"
    >|= order_return_of_json

  let new_order new_order =
    post_it ~body:(string_of_new_order new_order) "/order"
    >|= order_return_of_json

  let new_bulk_order (l : new_order list) =
    let rec combine ?(acc = "[") sl =
      match sl with
      | [] -> acc ^ "]"
      | hd :: [] -> acc ^ hd ^ "]"
      | hd :: tl -> combine ~acc:(acc ^ hd ^ ",") tl
    in
    let all_orders = List.map (string_of_new_order) l |> combine in
    post_it ~body:("{\"orders\" : " ^ all_orders ^ "}") "/order/bulk"
    >|= Yojson.Basic.Util.to_list
    >|= List.map (order_return_of_json)

  (***** PRIVATE PUT FUNCTIONS *****)

  (***** PRIVATE DELETE FUNCTIONS *****)

  let cancel_order order_id =
    delete_it ("/order?orderID=" ^ order_id)

end

(* WebsocketManager creates and maintains realtime level 2 tables *)
module WebsocketManager (U : AccountInfo) =
struct

  exception ParseFailure of string

  let ws_addr =
    Uri.of_string
      "wss://www.bitmex.com/realtime?subscribe=orderBookL2:XBTUSD"
      (*,instrument:XBTUSD,trade:XBTUSD*)

  type info = {
    info : string;
    version : string;
    timestamp : string;
    docs : string;
    heartbeat_enabled : bool
  }

  type subscribe_request_data = {
    op : string;
    args : string
  }

  type subscribe_info = {
    success : bool;
    subscribe : string;
    request : subscribe_request_data
  }

  (*type symbol_data = {
    symbol : symbol;
    open_value : int;
    fair_price : float;
    mark_price : float;
    indicative_settle_price : float;
    timestamp : string
    }*)

  type lvl_2_orderbook_entry = {
    symbol : symbol;
    id : int;
    side : order_side;
    size : int;
    price : float;
  }

  type lvl_2_orderbook_update = {
    symbol : symbol;
    id : int;
    side: order_side;
    size : int option;
    price : float option;
  }

  type lvl_2_orderbook_insert = {
    symbol : symbol;
    id : int;
    side: order_side;
    size : int;
    price : float;
  }

  type lvl_2_orderbook_delete = {
    symbol : symbol;
    id : int;
    side : order_side;
  }

  let orderbook : lvl_2_orderbook_entry list ref = ref []

  (* JSON data sent when subscribed to orderBook *)
  (* NOTE: Leaving out some non-critical fields: types, symbol, foreignkeys, attributes *)
  type orderbook_data = {
    table : string;
    keys : string list option;
    action : string option;
    data : lvl_2_orderbook_entry list
  }

  type orderbook_data_update = {
    table : string;
    keys : string list option;
    action : string option;
    data : lvl_2_orderbook_update list
  }

  type orderbook_data_insert = {
    table : string;
    action : string;
    data : lvl_2_orderbook_insert list
  }

  type orderbook_data_delete = {
    table : string;
    keys : string list option;
    action : string option;
    data : lvl_2_orderbook_delete list
  }

  (* JSON data sent when subscribed to instrument *)
  (* NOTE: Leaving out some non-critical fields: types and filter *)
  type instrument_data = {
    table : string;
    keys : string list option
  }

  type instrument_data_update_data = {
    symbol : symbol;
    total_volume : int;
    volume : int;
    total_turnover : int;
    turnover : int;
    open_interest : int;
    open_value : int;
    timestamp : string
  }

  type instrument_data_update = {
    table : string;
    action : string;
    data : instrument_data_update_data list
  }

  type trade_data' = {
    timestamp : string;
    symbol : symbol;
    side : string;
    size : int;
    price : float;
    tick_direction : string;
    trd_match_id : string;
    gross_value : int;
    home_notional : float;
    foreign_notional : int
  }

  type trade_data = {
    table : string;
    action : string;
    data : trade_data' list
  }

  (* Types of JSON data that are sent from the exchange *)
  type websocket_data =
    | Info of info
    | SubscribeInfo of subscribe_info
    | OrderbookData of orderbook_data
    | OrderbookUpdate of orderbook_data_update
    | OrderbookInsert of orderbook_data_insert
    | OrderbookDelete of orderbook_data_delete
    | InstrumentData of instrument_data
    | InstrumentDataUpdate of instrument_data_update
    | TradeData of trade_data

  let info_of_json json =
    let open Yojson.Basic.Util in
    Info {
      info = member "info" json |> to_string;
      version = member "version" json |> to_string;
      timestamp = member "timestamp" json |> to_string;
      docs = member "docs" json |> to_string;
      (* TODO: Seems to be an API issue where this is not always passed. Need to handle better. *)
      heartbeat_enabled = false (*member "heartbeatEnabled" json |> to_bool*)
    }

  let subscribe_info_of_json json =
    let open Yojson.Basic.Util in
    let req = member "request" json in
    SubscribeInfo {
      success = member "success" json |> to_bool;
      subscribe = member "subscribe" json |> to_string;
      request = {
        op = member "op" req |> to_string;
        args = member "args" req |> to_string
      }
    }

  (*let symbol_data_of_json json =
    let open Yojson.Basic.Util in
    SymbolData {
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      open_value = member "openValue" json |> to_int;
      fair_price = member "fairPrice" json |> to_float;
      mark_price = member "markPrice" json |> to_float;
      indicative_settle_price = member "indicativeSettlePrice" json |> to_float;
      timestamp = member "timestamp" json |> to_string
    }*)

  let (lvl_2_orderbook_entry_of_json : Yojson.Basic.json -> lvl_2_orderbook_entry) json =
    let open Yojson.Basic.Util in
    {
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      id = member "id" json |> to_int;
      side = member "side" json |> to_string |> order_side_of_string;
      size = (
        match member "size" json with
        | `Int i as n -> to_int n
        | _ -> failwith (
            "Error (1): lvl_2_orderbook_entry_of_json unable to parse " ^
            (Yojson.Basic.to_string json)
          )
      );
      price = (
        match member "price" json with
        | `Int i as n -> to_int n |> float_of_int
        | `Float f as n -> to_float n
        | _ -> failwith (
            "Error (2): lvl_2_orderbook_entry_of_json unable to parse " ^
            (Yojson.Basic.to_string json)
          )
      )
    }

  let (lvl_2_orderbook_update_of_json : Yojson.Basic.json -> lvl_2_orderbook_update) json =
    let open Yojson.Basic.Util in
    {
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      id = member "id" json |> to_int;
      side = member "side" json |> to_string |> order_side_of_string;
      size = (
        match member "size" json with
        | `Null -> None
        | `Int i as n -> Some (to_int n)
        | _ -> raise (ParseFailure (
            "Error (1): lvl_2_orderbook_update_of_json unable to parse " ^
            (Yojson.Basic.to_string json)
          ))
      );
      price = (
        match member "price" json with
        | `Null -> None
        | `Int i as n -> Some (to_int n |> float_of_int)
        | `Float f as n -> Some (to_float n)
        | _ -> raise (ParseFailure (
            "Error (2): lvl_2_orderbook_update_of_json unable to parse " ^
            (Yojson.Basic.to_string json)
          ))
      )
    }

  let (lvl_2_orderbook_insert_of_json : Yojson.Basic.json -> lvl_2_orderbook_insert) json =
    let open Yojson.Basic.Util in
    {
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      id = member "id" json |> to_int;
      side = member "side" json |> to_string |> order_side_of_string;
      size = member "size" json |> to_int;
      price = (
        match member "price" json with
        | `Int i as n -> to_int n |> float_of_int
        | `Float f as n -> to_float n
        | _ -> failwith "Error: lvl_2_orderbook_insert_of_json could not parse the price!"
      )
    }

  let (lvl_2_orderbook_delete_of_json : Yojson.Basic.json -> lvl_2_orderbook_delete) json =
    let open Yojson.Basic.Util in
    {
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      id = member "id" json |> to_int;
      side = member "side" json |> to_string |> order_side_of_string;
    }

  let orderbook_data_of_json json =
    let open Yojson.Basic.Util in
    try (
      OrderbookData {
        table = member "table" json |> to_string;
        keys = (
          match member "keys" json with
          | `Null -> None
          | `List l as k -> Some (to_list k |> List.map to_string)
          | _ -> raise (ParseFailure (
              "Error (1): orderbook_data_of_json unable to parse " ^
              (Yojson.Basic.to_string json)
            ))
        );
        action = (
          let act = member "action" json in
          if act = `Null
          then None
          else Some (to_string act));
        data = member "data" json |> to_list |> List.map lvl_2_orderbook_entry_of_json;
      }
    )
    with
    | _ ->
      failwith ("Error (2): orderbook_data_of_json unable to parse " ^ (Yojson.Basic.to_string json))

  let orderbook_update_of_json json =
    let open Yojson.Basic.Util in
    try (
      OrderbookUpdate {
        table = member "table" json |> to_string;
        keys = (
          match member "keys" json with
          | `Null -> None
          | `List l as k -> Some (to_list k |> List.map to_string)
          | _ -> raise (ParseFailure (
              "Error: orderbook_update_of_json unable to parse " ^
              (Yojson.Basic.to_string json)
            ))
        );
        action = (
          let act = member "action" json in
          if act = `Null
          then None
          else Some (to_string act));
        data = member "data" json |> to_list |> List.map lvl_2_orderbook_update_of_json;
      }
    )
    with
    | _ ->
      failwith (
        "Error: orderbook_update_of_json unable to parse " ^
        (Yojson.Basic.to_string json)
      )

  let orderbook_delete_of_json json =
    let open Yojson.Basic.Util in
    try (
      OrderbookDelete {
        table = member "table" json |> to_string;
        keys = (
          match member "keys" json with
          | `Null -> None
          | `List l as k -> Some (to_list k |> List.map to_string)
          | _ -> raise (ParseFailure (
              "Error: orderbook_update_of_json unable to parse " ^
              (Yojson.Basic.to_string json)
            ))
        );
        action = (
          let act = member "action" json in
          if act = `Null
          then None
          else Some (to_string act));
        data = member "data" json |> to_list |> List.map lvl_2_orderbook_delete_of_json;
      }
    )
    with
    | _ ->
      failwith (
        "Error: orderbook_update_of_json unable to parse " ^
        (Yojson.Basic.to_string json)
      )

  let orderbook_insert_of_json json =
    let open Yojson.Basic.Util in
    try (
      OrderbookInsert {
        table = member "table" json |> to_string;
        action = member "action" json |> to_string;
        data = member "data" json |> to_list |> List.map lvl_2_orderbook_insert_of_json;
      }
    )
    with
    | _ -> failwith (
        "Error: orderbook_insert_of_json unable to parse " ^
        (Yojson.Basic.to_string json)
    )

  let instrument_data_of_json json =
    let open Yojson.Basic.Util in
    try (
      InstrumentData {
        table = member "table" json |> to_string;
        keys = (
          match member "keys" json with
          | `Null -> None
          | `List l as k -> Some (to_list k |> List.map to_string)
          | _ ->
            raise (ParseFailure (
              "Error: instrument_data_of_json unable to parse " ^
              (Yojson.Basic.to_string json)
            ))
        )
      }
    )
    with
    | _ ->
      failwith (
        "Error: instrument_data_of_json unable to parse " ^
        (Yojson.Basic.to_string json)
      )

  let instrument_data_update_data_of_json json =
    let open Yojson.Basic.Util in
    {
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      total_volume = member "totalVolume" json |> to_int;
      volume = member "volume" json |> to_int;
      total_turnover = member "totalTurnover" json |> to_int;
      turnover = member "turnover" json |> to_int;
      open_interest = member "openInterest" json |> to_int;
      open_value = member "openValue" json |> to_int;
      timestamp = member "timestamp" json |> to_string
    }

  let instrument_data_update_of_json json =
    let open Yojson.Basic.Util in
    try (
      InstrumentDataUpdate {
        table = member "table" json |> to_string;
        action = member "action" json |> to_string;
        data = member "data" json
          |> to_list
          |> List.map instrument_data_update_data_of_json
      }
    )
    with
    | Failure "hd" -> failwith ("Error: List.hd - instrument_data_update_of_json")
    | _ ->
      failwith (
        "Error: instrument_data_update_of_json unable to parse " ^
        (Yojson.Basic.to_string json)
      )

  let trade_data'_of_json json =
    let open Yojson.Basic.Util in
    try ({
      timestamp = member "timestamp" json |> to_string;
      symbol = member "symbol" json |> to_string |> symbol_of_string;
      side = member "side" json |> to_string;
      size = member "size" json |> to_int;
      price = (
        match member "price" json with
        | `Int i -> float_of_int i
        | `Float f -> f
        | _ -> raise (ParseFailure (
            "Error: trade_data'_of_json unable to parse " ^
            (Yojson.Basic.to_string json)
          ))
      );
      tick_direction = member "tickDirection" json |> to_string;
      trd_match_id = member "trdMatchID" json |> to_string;
      gross_value = member "grossValue" json |> to_int;
      home_notional = member "homeNotional" json |> to_float;
      foreign_notional = member "foreignNotional" json |> to_int
    })
     with
    | _ -> failwith ("Error: trade_data'_of_json unable to parse " ^ (Yojson.Basic.to_string json))

  let trade_data_of_json json =
    let open Yojson.Basic.Util in
    try (
      TradeData {
        table = member "table" json |> to_string;
        action = member "action" json |> to_string;
        data = member "data" json |> to_list |> List.map trade_data'_of_json;
      }
    )
    with
    | _ -> failwith ("Error: trade_data_of_json unable to parse " ^ (Yojson.Basic.to_string json))

   let websocket_data_of_string s =
    let open Yojson.Basic.Util in
    let open Utils in
    let json = Yojson.Basic.from_string s in
    match member "info" json, member "success" json, member "table" json, member "action" json with
    | `String _, `Null, `Null, `Null -> info_of_json json
    | `Null, `Bool true, `Null, `Null -> subscribe_info_of_json json
    | `Null, `Null, `String "orderBookL2", `String "partial" -> orderbook_data_of_json json
    | `Null, `Null, `String "orderBookL2", `String "delete" -> orderbook_delete_of_json json
    | `Null, `Null, `String "orderBookL2", `String "insert" -> orderbook_insert_of_json json
    | _, _, `String "orderBookL2", `String "update" -> orderbook_update_of_json json
    | `Null, `Null, `String "instrument", `String "partial" -> instrument_data_of_json json
    | _, _, `String "instrument", `String "update" -> instrument_data_update_of_json json
    | `Null, `Null, `String "trade", `String "insert" -> trade_data_of_json json
    | `Null, `Null, `String "trade", `String "partial" -> trade_data_of_json json
    | _ ->
      failwith (
        "Error: websocket_data_of_string cannot parse json.\n" ^
        "Match statement found " ^
        (string_of_option @@ to_string_option (member "info" json)) ^ ", " ^
        (string_of_bool_option @@ to_bool_option (member "success" json)) ^ ", " ^
        (string_of_option @@ to_string_option (member "table" json)) ^ ", " ^
        (string_of_option @@ to_string_option (member "action" json))
        )

  let orderbook_sort (x : lvl_2_orderbook_entry) (y : lvl_2_orderbook_entry) =
    if x.price < y.price
    then -1
    else (
      if x.price = y.price
      then 0
      else 1
    )

  (* Apply an single lvl_2_orderbook_update to the orderbook *)
  (*
     From the API docs:
     Upon subscription, you will receive an image of the existing data, so you can get started.
     This comes through as a partial action.
     You may receive other messages before the partial comes through.
     In that case, drop any messages received until you have received the partial.
  *)
  let update_orderbook (x : lvl_2_orderbook_update) =
    (* Get the old entry to be replaced/updated *)
    let old_entry =
      try
        List.filter (fun (entry : lvl_2_orderbook_entry) -> entry.id = x.id) !orderbook
        |> List.hd |> fun x -> Some x
      with
        | Failure "hd" -> None (*failwith ("Error: update_orderbook - Failure List.hd")*)
    in
    match old_entry with
    | None -> ()
    | Some x ->
      let new_entry : lvl_2_orderbook_entry = {
        symbol = x.symbol;
        id = x.id;
        side = x.side;
        size = x.size;
        price = x.price;
      }
      in
      (* Drop the old entry from the orderbook *)
      List.filter (fun (ob_entry : lvl_2_orderbook_entry) -> ob_entry.id != x.id) !orderbook
      (* Append the update *)
      |> fun (l : lvl_2_orderbook_entry list) -> (new_entry :: l)
      (* Sort *)
      |> fun (l : lvl_2_orderbook_entry list) -> List.sort orderbook_sort l
      |> fun (l : lvl_2_orderbook_entry list) -> orderbook := l

  (* Do nothing until the first "partial" action has been received *)
  let insert_orderbook (x : lvl_2_orderbook_insert) =
    match !orderbook with
    | [] -> ()
    | _ ->
      let new_entry : lvl_2_orderbook_entry = {
        symbol = x.symbol;
        id = x.id;
        side = x.side;
        size = x.size;
        price = x.price;
      }
      in
      let new_orderbook = new_entry :: !orderbook in
      orderbook := (List.sort orderbook_sort new_orderbook)

  (* Apply the delete action to the current orderbook *)
  let delete_orderbook (x : lvl_2_orderbook_delete) =
    orderbook := List.filter
        (fun (ob_entry : lvl_2_orderbook_entry) -> ob_entry.id != x.id) !orderbook

  let apply_websocket_feed (x : websocket_data) =
    match x with
    | Info _ -> ()
    | SubscribeInfo _ -> ()
    | OrderbookData od -> (
        orderbook := od.data (* Reset the orderbook *);
        print_string "orderbook has been set!"
      )
    | OrderbookUpdate ou -> List.iter (update_orderbook) ou.data
    | OrderbookInsert oi -> List.iter (insert_orderbook) oi.data
    | OrderbookDelete od -> List.iter (delete_orderbook) od.data
    | InstrumentData _ -> ()
    | InstrumentDataUpdate _ -> ()
    | TradeData td -> print_string "TODO: NEED TO HANDLE THIS!"

  let best_bid_price () =
    let best_price acc (x : lvl_2_orderbook_entry) =
      match x.side with
      | Buy -> max acc x.price
      | Sell -> acc
    in
    match !orderbook with
    | [] -> failwith ("Error: best_bid_price - No data in orderbook")
    | _ ->
      List.fold_left (fun acc (x : lvl_2_orderbook_entry) -> best_price acc x) min_float !orderbook

  let best_bid_size () =
    match !orderbook with
    | [] -> failwith ("Error: best_bid_size - No data in orderbook")
    | _ -> (
        let high_bid = best_bid_price () in
        List.filter (fun (x : lvl_2_orderbook_entry) -> x.price = high_bid) !orderbook
        |> List.hd
        |> fun x -> x.size
      )

  let best_ask_price () =
    let best_price acc (x : lvl_2_orderbook_entry) =
      match x.side with
      | Sell -> min acc x.price
      | Buy -> acc
    in
    match !orderbook with
    | [] -> failwith ("Error: best_ask_price - No data in orderbook")
    | _ ->
      List.fold_left (fun acc (x : lvl_2_orderbook_entry) -> best_price acc x) max_float !orderbook

  let best_ask_size () =
    match !orderbook with
    | [] -> failwith ("Error: best_ask_size - No data in orderbook")
    | _ -> (
        let low_ask = best_ask_price () in
        List.filter (fun (x : lvl_2_orderbook_entry) -> x.price = low_ask) !orderbook
        |> List.hd
        |> fun x -> x.size
      )

  (* Set up the websocket connection *)
  let ws_conn () = Websocket.open_connection ws_addr

  (* push function *)
  let push_fun (_, push) ~msg =
    let frame = Websocket.Frame.of_string msg in
    Lwt.return @@ push (Some frame)

  (* Subscribed websocket *)
  let subscribed_ws () =
    ws_conn ()
    (*>>= fun ((a, b) : (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit))) ->
      push_fun (a, b) ~msg:subscribe_msg (* returns unit *)
      >>= fun () -> Lwt.return (a, b) *)

  (* get stream element and print to screen *)
  let get_element (stream, _) =
    let print_reply (x : Websocket.Frame.t) =
      let s = Websocket.Frame.content x in
      Lwt_io.print (s ^ "\n\n")
    in
    Lwt_stream.next stream
    >>= print_reply

  let log_data oc s =
    match oc with
    | None -> ()
    | Some out_chan -> (
        output_string out_chan s;
        flush out_chan
      )

  (* Fix a string like "10." to be "10.0" *)
  let rec append_zeros s =
    let l = Str.split (Str.regexp "[.]") s in
    if List.length l = 1
    then s ^ "0"
    else s

  (* Fix the size of a float to two decimal places*)
  let fix_float f = f -. mod_float f 0.01

  let get_price ~side n =
    try
      let ob_side =
        List.filter (fun (ob_entry : lvl_2_orderbook_entry) -> ob_entry.side = side) !orderbook
      in
      match side, ob_side with
      | _, [] -> "---"
      | Buy, _ -> append_zeros @@ string_of_float (List.nth (List.rev ob_side) n).price
      | Sell, _ -> append_zeros @@ string_of_float (List.nth ob_side n).price
    with
      | _ -> "get_price fail"

  let refresh_gui num_updates =
    let open Gui in

    let get_size ~side n =
      try
        let ob_side =
          List.filter (fun (ob_entry : lvl_2_orderbook_entry) -> ob_entry.side = side) !orderbook
        in
        match side, ob_side with
        | _, [] -> "---"
        | Buy, _ -> string_of_int (List.nth (List.rev ob_side) n).size
        | Sell, _ -> string_of_int (List.nth ob_side n).size
      with
        | _ -> "get_size fail"
    in

    let get_spread () =
      try
        float_of_string (get_price Sell 0) -. float_of_string (get_price Buy 0)
        |> fix_float
        |> string_of_float
        |> append_zeros
      with
      | _ -> "get_spread fail"
      (*| _ -> failwith ("Error: get_spread failed!")*)
    in

    spread_label_lvl_2#set_text @@ get_spread ();

    bid_size_1#set_text @@ get_size ~side:Buy 0;
    bid_size_2#set_text @@ get_size ~side:Buy 1;
    bid_size_3#set_text @@ get_size ~side:Buy 2;
    bid_size_4#set_text @@ get_size ~side:Buy 3;
    bid_size_5#set_text @@ get_size ~side:Buy 4;
    bid_size_6#set_text @@ get_size ~side:Buy 5;
    bid_size_7#set_text @@ get_size ~side:Buy 6;
    bid_size_8#set_text @@ get_size ~side:Buy 7;
    bid_size_9#set_text @@ get_size ~side:Buy 8;
    bid_size_10#set_text @@ get_size ~side:Buy 9;

    bid_price_1#set_text @@ get_price ~side:Buy 0;
    bid_price_2#set_text @@ get_price ~side:Buy 1;
    bid_price_3#set_text @@ get_price ~side:Buy 2;
    bid_price_4#set_text @@ get_price ~side:Buy 3;
    bid_price_5#set_text @@ get_price ~side:Buy 4;
    bid_price_6#set_text @@ get_price ~side:Buy 5;
    bid_price_7#set_text @@ get_price ~side:Buy 6;
    bid_price_8#set_text @@ get_price ~side:Buy 7;
    bid_price_9#set_text @@ get_price ~side:Buy 8;
    bid_price_10#set_text @@ get_price ~side:Buy 9;

    ask_size_1#set_text @@ get_size ~side:Sell 0;
    ask_size_2#set_text @@ get_size ~side:Sell 1;
    ask_size_3#set_text @@ get_size ~side:Sell 2;
    ask_size_4#set_text @@ get_size ~side:Sell 3;
    ask_size_5#set_text @@ get_size ~side:Sell 4;
    ask_size_6#set_text @@ get_size ~side:Sell 5;
    ask_size_7#set_text @@ get_size ~side:Sell 6;
    ask_size_8#set_text @@ get_size ~side:Sell 7;
    ask_size_9#set_text @@ get_size ~side:Sell 8;
    ask_size_10#set_text @@ get_size ~side:Sell 9;

    ask_price_1#set_text @@ get_price ~side:Sell 0;
    ask_price_2#set_text @@ get_price ~side:Sell 1;
    ask_price_3#set_text @@ get_price ~side:Sell 2;
    ask_price_4#set_text @@ get_price ~side:Sell 3;
    ask_price_5#set_text @@ get_price ~side:Sell 4;
    ask_price_6#set_text @@ get_price ~side:Sell 5;
    ask_price_7#set_text @@ get_price ~side:Sell 6;
    ask_price_8#set_text @@ get_price ~side:Sell 7;
    ask_price_9#set_text @@ get_price ~side:Sell 8;
    ask_price_10#set_text @@ get_price ~side:Sell 9;

    (*let t = Unix.localtime !last_heartbeat_time in
    let f = string_of_int in
    let open Unix in
      heartbeat_label#set_text ((f (t.tm_hour mod 12)) ^ ":" ^ (f t.tm_min) ^ ":" ^ (f t.tm_sec));*)

    let t = Unix.localtime @@ Unix.time () in
    let f = string_of_int in
    let open Unix in
      refresh_label#set_text ((f (t.tm_hour mod 12)) ^ ":" ^ (f t.tm_min) ^ ":" ^ (f t.tm_sec));

    num_updates_label#set_text (string_of_int num_updates);

    Lwt.return ()

  (* This indicates if the first "partial" message has been received from the websocket.  *)
  (* You must ignor all websocket messages until the first "partial" message is received. *)
  let partial_received = ref false

  let main () =

    Lwt.async(fun () -> Lwt_io.print "\nEntered main\n");

    Lwt.async (Gui.main);

    Lwt.async(fun () -> Lwt_io.print "\nStarted Gui.main\n");

    let oc =
      match U.data_loc with
      | None -> None
      | Some path -> Some (open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o640 path)
    in

    Lwt.async(fun () ->
        match U.data_loc with
        | None -> Lwt_io.print ("\nNot writing websocket output\n")
        | Some s -> Lwt_io.print ("\nWriting websocket output to" ^ s ^ "\n")
      );

    (* Subscribe to the websocket *)
    lwt (ws_stream, ws_push) = subscribed_ws () in

    Lwt.async (fun () -> Lwt_io.print "\nSubscribed to websocket\n");

    while_lwt true do

      (* Need to sleep for the GUI to function, not sure why? *)
      lwt () = Lwt_unix.sleep 0.1 in

      let websocket_messages = Lwt_stream.get_available ws_stream in

      lwt () =
        (*Lwt_stream.get_available ws_stream
          |>*) List.iter (fun f ->
            let websocket_data = Websocket.Frame.content f in
            log_data oc (websocket_data ^ "\n\n");
          websocket_data_of_string websocket_data |> apply_websocket_feed;
        ) websocket_messages
        |> Lwt.return
      in

      refresh_gui (List.length websocket_messages)

    done

end
