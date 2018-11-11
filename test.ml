open OUnit
open Bitmex
open BitmexTypes

module MyAccountInfo : AccountInfo =
struct
  let username = "my_username"
  let api_key = "my_api_key"
  let secret_key = "my_secret_key"
  let data_loc = None
  let max_lots = 1
  let live_trade = false
end

module WS = WebsocketManager (MyAccountInfo)

open WS

let test_symbol_of_string test_ctx =
  assert_equal (symbol_of_string "XBTUSD") XBTUSD;
  assert_equal (symbol_of_string "Future") (Other "Future")

(* info_of_json *)
let test_info_of_json test_ctx =
  assert_equal
    (info_of_json @@ Yojson.Basic.from_string (
      "{" ^
      "\"info\":\"Welcome to the BitMEX Realtime API.\"," ^
      "\"version\":\"1.2.0\"," ^
      "\"timestamp\":\"2017-06-10T21:59:22.397Z\"," ^
      "\"docs\":\"https://www.bitmex.com/app/wsAPI\"," ^
      "\"heartbeatEnabled\":false" ^
      "}"
    ))
    (Info {
      info = "Welcome to the BitMEX Realtime API.";
      version = "1.2.0";
      timestamp = "2017-06-10T21:59:22.397Z";
      docs = "https://www.bitmex.com/app/wsAPI";
      heartbeat_enabled = false
    })

(* subscribe_info_of_json *)
let test_subscribe_info_of_json test_ctx =
  assert_equal
    (WS.subscribe_info_of_json @@ Yojson.Basic.from_string (
      "{" ^
      "\"success\":true," ^
      "\"subscribe\":\"instrument\"," ^
      "\"request\":{\"op\":\"subscribe\",\"args\":\"instrument\"}" ^
      "}"
    ))
    (SubscribeInfo {
      success = true;
      subscribe = "instrument";
      request = {op = "subscribe"; args = "instrument"}
    })

let test_3 test_ctx =
  assert_equal
    (subscribe_info_of_json @@ Yojson.Basic.from_string (
      "{" ^
      "\"success\":true," ^
      "\"subscribe\":\"orderBook:XBTUSD\"," ^
      "\"request\":{\"op\":\"subscribe\",\"args\":\"orderBook:XBTUSD\"}" ^
      "}"
    ))
    (SubscribeInfo {
      success = true;
      subscribe = "orderBook:XBTUSD";
      request = {op = "subscribe"; args = "orderBook:XBTUSD"}
    })

(* symbol_data_of_json *)
(*let test_4 text_ctx =
  assert_equal
    (symbol_data_of_json @@ Yojson.Basic.from_string (
      "{" ^
        "\"symbol\":\"XBTUSD\"," ^
        "\"openValue\":723430450596," ^
        "\"fairPrice\":2958.9," ^
        "\"markPrice\":2958.9," ^
        "\"indicativeSettlePrice\":2955.67," ^
        "\"timestamp\":\"2017-06-11T16:23:50.000Z\"" ^
      "}"
    ))
    (SymbolData {
      symbol = XBTUSD;
      open_value = 723430450596;
      fair_price = 2958.9;
      mark_price = 2958.9;
      indicative_settle_price = 2955.67;
      timestamp = "2017-06-11T16:23:50.000Z"
    })
*)
(*
      "{" ^
      "\"table\":\"instrument\"," ^
      "\"action\:\update\"," ^
      "\"data\":[{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"openValue\":723430450596," ^
          "\"fairPrice\":2958.9," ^
          "\"markPrice\":2958.9," ^
          "\"indicativeSettlePrice\":2955.67," ^
          "\"timestamp\":\"2017-06-11T16:23:50.000Z\"" ^
        "}]" ^
      "}"
*)

(* lvl_2_entry_of_json *)
let test_5 text_ctx =
  assert_equal
    (lvl_2_orderbook_entry_of_json @@ Yojson.Basic.from_string (
      "{" ^
        "\"symbol\":\"XBTUSD\"," ^
        "\"id\":1234," ^
        "\"side\":\"Buy\"," ^
        "\"size\":2977," ^
        "\"price\":3000" ^
      "}"
    ))
    ({
      symbol = XBTUSD;
      id = 1234;
      side = Buy;
      size = 2977;
      price = 3000.0;
    })

(* orderbook_data_of_json - without keys *)
let test_6 test_ctx =
  assert_equal
    (orderbook_data_of_json @@ Yojson.Basic.from_string (
      "{" ^
        "\"table\":\"orderBookL2\"," ^
        "\"action\":\"update\"," ^
        "\"data\":[" ^
          "{" ^
            "\"symbol\":\"XBTUSD\"," ^
            "\"id\":1234," ^
            "\"side\":\"Sell\"," ^
            "\"size\":2978," ^
            "\"price\":2979.6" ^
          "}," ^
          "{" ^
            "\"symbol\":\"XBTUSD\"," ^
            "\"id\":15649," ^
            "\"side\":\"Buy\"," ^
            "\"size\":2978," ^
            "\"price\":2979.8" ^
        "}]" ^
      "}"
        ))
    (OrderbookData {
      table = "orderBookL2";
      keys = None;
      action = Some "update";
      data  = [
        {
          symbol = XBTUSD;
          id = 1234;
          side = Sell;
          size = 2978;
          price = 2979.6;
        };
        {
          symbol = XBTUSD;
          id = 15649;
          side = Buy;
          size = 2978;
          price = 2979.8;
        }
      ]
  })

(* orderbook_data_of_json - with keys *)
let test_7 test_ctx =
  assert_equal
    (orderbook_data_of_json @@ Yojson.Basic.from_string (
        "{" ^
        "\"table\":\"orderBookL2\"," ^
        "\"keys\":[\"symbol\",\"level\"]," ^
        "\"types\":{" ^
          "\"symbol\":\"symbol\"," ^
          "\"level\":\"long\"," ^
          "\"bidSize\":\"long\"," ^
          "\"bidPrice\":\"float\"," ^
          "\"askPrice\":\"float\"," ^
          "\"askSize\":\"long\"," ^
          "\"timestamp\":\"timestamp\"" ^
        "}," ^
        "\"foreignKeys\":{\"symbol\":\"instrument\"}," ^
        "\"attributes\":{\"symbol\":\"sorted\"}," ^
        "\"action\":\"partial\"," ^
        "\"data\":[" ^
          "{" ^
            "\"symbol\":\"XBTUSD\"," ^
            "\"id\":1234," ^
            "\"side\":\"Buy\", " ^
            "\"size\":2977," ^
            "\"price\":2978.8" ^
          "}," ^
          "{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":1235," ^
          "\"side\":\"Sell\", " ^
          "\"size\":2980," ^
          "\"price\":2111.1" ^
        "}]" ^
        "}"
      ))
    (OrderbookData {
      table = "orderBookL2";
      keys = Some ["symbol"; "level"];
      action = Some "partial";
      data  = [
        {
          symbol = XBTUSD;
          id = 1234;
          side = Buy;
          size = 2977;
          price = 2978.8;
        };
        {
          symbol = XBTUSD;
          id = 1235;
          side = Sell;
          size = 2980;
          price = 2111.1;
        }
      ]
      }
    )

(* websocket_data_of_string *)
let test_websocket_data_of_string test_ctx =
  assert_equal
    (websocket_data_of_string (
      "{" ^
      "\"table\":\"orderBookL2\"," ^
      "\"keys\":[\"symbol\",\"level\"]," ^
      "\"types\":{" ^
        "\"symbol\":\"symbol\"," ^
        "\"level\":\"long\"," ^
        "\"bidSize\":\"long\"," ^
        "\"bidPrice\":\"float\"," ^
        "\"askPrice\":\"float\"," ^
        "\"askSize\":\"long\"," ^
        "\"timestamp\":\"timestamp\"" ^
      "}," ^
      "\"foreignKeys\":{\"symbol\":\"instrument\"}," ^
      "\"attributes\":{\"symbol\":\"sorted\"}," ^
      "\"action\":\"partial\"," ^
      "\"data\":[{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":1234," ^
          "\"side\":\"Buy\"," ^
          "\"size\":100," ^
          "\"price\":2973.3" ^
        "}," ^
        "{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":1235," ^
          "\"side\":\"Sell\"," ^
          "\"size\":2972," ^
          "\"price\":2973.4" ^
        "}]}"
      ))
      (OrderbookData {
        table = "orderBookL2";
        keys = Some ["symbol"; "level"];
        action = Some "partial";
        data  = [
          {
            symbol = XBTUSD;
            id = 1234;
            side = Buy;
            size = 100;
            price = 2973.3;
          };
          {
            symbol = XBTUSD;
            id = 1235;
            side = Sell;
            size = 2972;
            price = 2973.4;
          }
        ]
        }
      )

(* websocket_data_of_string *)
let test_9 test_ctx =
  assert_equal
    (websocket_data_of_string (
      "{" ^
        "\"table\":\"orderBookL2\"," ^
        "\"action\":\"update\"," ^
        "\"data\":[" ^
        "{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":1234," ^
          "\"side\":\"Buy\"," ^
          "\"size\":1235," ^
          "\"price\":2973.1" ^
        "}," ^
        "{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":1234," ^
          "\"side\":\"Sell\"," ^
          "\"size\":1200," ^
          "\"price\":2973.2" ^
        "}]}"
      ))
    (OrderbookUpdate {
        table = "orderBookL2";
        keys = None;
        action = Some "update";
        data  = [
          {
            symbol = XBTUSD;
            id = 1234;
            side = Buy;
            size = Some 1235;
            price = Some 2973.1;
          };
          {
            symbol = XBTUSD;
            id = 1234;
            side = Sell;
            size = Some 1200;
            price = Some 2973.2;
          }
        ]
        }
    )

(* websocket_data_of_string *)
let test_websocket_data_of_string_2 test_ctx =
  assert_equal
    (websocket_data_of_string (
        "{" ^
          "\"success\":true," ^
          "\"subscribe\":\"orderBook:XBTUSD\"," ^
          "\"request\":{\"op\":\"subscribe\",\"args\":\"orderBook:XBTUSD\"}" ^
        "}"
      )
    )
    (SubscribeInfo {
      success = true;
      subscribe = "orderBook:XBTUSD";
      request = {
        op = "subscribe";
        args = "orderBook:XBTUSD"
      }
    })

(* websocket_data_of_string *)
let test_websocket_data_of_string_3 test_ctx =
  assert_equal
    (websocket_data_of_string (
        "{" ^
        "\"table\":\"instrument\"," ^
        "\"keys\":[\"symbol\"]," ^
        "\"types\":{" ^
          "\"symbol\":\"symbol\"," ^
          "\"rootSymbol\":\"symbol\"," ^
          "\"state\":\"symbol\"," ^
          "\"typ\":\"symbol\"," ^
          "\"listing\":\"timestamp\"," ^
          "\"front\":\"timestamp\"," ^
          "\"expiry\":\"timestamp\"," ^
          "\"settle\":\"timestamp\"," ^
          "\"relistInterval\":\"timespan\"," ^
          "\"inverseLeg\":\"symbol\"," ^
          "\"sellLeg\":\"symbol\"," ^
          "\"buyLeg\":\"symbol\"," ^
          "\"positionCurrency\":\"symbol\"," ^
          "\"underlying\":\"symbol\"," ^
          "\"quoteCurrency\":\"symbol\"," ^
          "\"underlyingSymbol\":\"symbol\"," ^
          "\"reference\":\"symbol\"," ^
          "\"referenceSymbol\":\"symbol\"," ^
          "\"calcInterval\":\"timespan\"," ^
          "\"publishInterval\":\"timespan\"," ^
          "\"publishTime\":\"timespan\"," ^
          "\"maxOrderQty\":\"long\"," ^
          "\"maxPrice\":\"float\"," ^
          "\"lotSize\":\"long\"," ^
          "\"tickSize\":\"float\"," ^
          "\"multiplier\":\"long\"," ^
          "\"settlCurrency\":\"symbol\"," ^
          "\"underlyingToPositionMultiplier\":\"long\"," ^
          "\"underlyingToSettleMultiplier\":\"long\"," ^
          "\"quoteToSettleMultiplier\":\"long\"," ^
          "\"isQuanto\":\"boolean\"," ^
          "\"isInverse\":\"boolean\"," ^
          "\"initMargin\":\"float\"," ^
          "\"maintMargin\":\"float\"," ^
          "\"riskLimit\":\"long\"," ^
          "\"riskStep\":\"long\"," ^
          "\"limit\":\"float\"," ^
          "\"capped\":\"boolean\"," ^
          "\"taxed\":\"boolean\"," ^
          "\"deleverage\":\"boolean\"," ^
          "\"makerFee\":\"float\"," ^
          "\"takerFee\":\"float\"," ^
          "\"settlementFee\":\"float\"," ^
          "\"insuranceFee\":\"float\"," ^
          "\"fundingBaseSymbol\":\"symbol\"," ^
          "\"fundingQuoteSymbol\":\"symbol\"," ^
          "\"fundingPremiumSymbol\":\"symbol\"," ^
          "\"fundingTimestamp\":\"timestamp\"," ^
          "\"fundingInterval\":\"timespan\"," ^
          "\"fundingRate\":\"float\"," ^
          "\"indicativeFundingRate\":\"float\"," ^
          "\"rebalanceTimestamp\":\"timestamp\"," ^
          "\"rebalanceInterval\":\"timespan\"," ^
          "\"openingTimestamp\":\"timestamp\"," ^
          "\"closingTimestamp\":\"timestamp\"," ^
          "\"sessionInterval\":\"timespan\"," ^
          "\"prevClosePrice\":\"float\"," ^
          "\"limitDownPrice\":\"float\"," ^
          "\"limitUpPrice\":\"float\"," ^
          "\"bankruptLimitDownPrice\":\"float\"," ^
          "\"bankruptLimitUpPrice\":\"float\"," ^
          "\"prevTotalVolume\":\"long\"," ^
          "\"totalVolume\":\"long\"," ^
          "\"volume\":\"long\"," ^
          "\"volume24h\":\"long\"," ^
          "\"prevTotalTurnover\":\"long\"," ^
          "\"totalTurnover\":\"long\"," ^
          "\"turnover\":\"long\"," ^
          "\"turnover24h\":\"long\"," ^
          "\"prevPrice24h\":\"float\"," ^
          "\"vwap\":\"float\"," ^
          "\"highPrice\":\"float\"," ^
          "\"lowPrice\":\"float\"," ^
          "\"lastPrice\":\"float\"," ^
          "\"lastPriceProtected\":\"float\"," ^
          "\"lastTickDirection\":\"symbol\"," ^
          "\"lastChangePcnt\":\"float\"," ^
          "\"bidPrice\":\"float\"," ^
          "\"midPrice\":\"float\"," ^
          "\"askPrice\":\"float\"," ^
          "\"impactBidPrice\":\"float\"," ^
          "\"impactMidPrice\":\"float\"," ^
          "\"impactAskPrice\":\"float\"," ^
          "\"hasLiquidity\":\"boolean\"," ^
          "\"openInterest\":\"long\"," ^
          "\"openValue\":\"long\"," ^
          "\"fairMethod\":\"symbol\"," ^
          "\"fairBasisRate\":\"float\"," ^
          "\"fairBasis\":\"float\"," ^
          "\"fairPrice\":\"float\"," ^
          "\"markMethod\":\"symbol\"," ^
          "\"markPrice\":\"float\"," ^
          "\"indicativeTaxRate\":\"float\"," ^
          "\"indicativeSettlePrice\":\"float\"," ^
          "\"settledPrice\":\"float\"," ^
          "\"timestamp\":\"timestamp\"" ^
        "}," ^
        "\"foreignKeys\":{" ^
          "\"inverseLeg\":\"instrument\"," ^
          "\"sellLeg\":\"instrument\"," ^
          "\"buyLeg\":\"instrument\"" ^
        "}," ^
        "\"attributes\":{}," ^
        "\"action\":\"partial\"," ^
        "\"data\":[{" ^
            "\"symbol\":\"XBTUSD\"," ^
            "\"rootSymbol\":\"XBT\"," ^
            "\"state\":\"Open\"," ^
            "\"typ\":\"FFWCSX\"," ^
            "\"listing\":\"2016-05-13T12:00:00.000Z\"," ^
            "\"front\":\"2016-05-13T12:00:00.000Z\"," ^
            "\"expiry\":null," ^
            "\"settle\":null," ^
            "\"relistInterval\":null," ^
            "\"inverseLeg\":\"\"," ^
            "\"sellLeg\":\"\"," ^
            "\"buyLeg\":\"\"," ^
            "\"positionCurrency\":\"USD\"," ^
            "\"underlying\":\"XBT\"," ^
            "\"quoteCurrency\":\"USD\"," ^
            "\"underlyingSymbol\":\"XBT=\"," ^
            "\"reference\":\"BMEX\"," ^
            "\"referenceSymbol\":\".BXBT\"," ^
            "\"calcInterval\":null," ^
            "\"publishInterval\":null," ^
            "\"publishTime\":null," ^
            "\"maxOrderQty\":10000000," ^
            "\"maxPrice\":1000000," ^
            "\"lotSize\":1," ^
            "\"tickSize\":0.1," ^
            "\"multiplier\":-100000000," ^
            "\"settlCurrency\":\"XBt\"," ^
            "\"underlyingToPositionMultiplier\":null," ^
            "\"underlyingToSettleMultiplier\":-100000000," ^
            "\"quoteToSettleMultiplier\":null," ^
            "\"isQuanto\":false," ^
            "\"isInverse\":true," ^
            "\"initMargin\":0.01," ^
            "\"maintMargin\":0.005," ^
            "\"riskLimit\":20000000000," ^
            "\"riskStep\":10000000000," ^
            "\"limit\":null," ^
            "\"capped\":false," ^
            "\"taxed\":true," ^
            "\"deleverage\":true," ^
            "\"makerFee\":-0.00025," ^
            "\"takerFee\":0.00075," ^
            "\"settlementFee\":0," ^
            "\"insuranceFee\":0," ^
            "\"fundingBaseSymbol\":\".XBTBON8H\"," ^
            "\"fundingQuoteSymbol\":\".USDBON8H\"," ^
            "\"fundingPremiumSymbol\":\".XBTUSDPI8H\"," ^
            "\"fundingTimestamp\":\"2017-06-15T04:00:00.000Z\"," ^
            "\"fundingInterval\":\"2000-01-01T08:00:00.000Z\"," ^
            "\"fundingRate\":-0.00233," ^
            "\"indicativeFundingRate\":-0.001331," ^
            "\"rebalanceTimestamp\":null," ^
            "\"rebalanceInterval\":null," ^
            "\"openingTimestamp\":\"2017-06-15T00:00:00.000Z\"," ^
            "\"closingTimestamp\":\"2017-06-15T02:00:00.000Z\"," ^
            "\"sessionInterval\":\"2000-01-01T02:00:00.000Z\"," ^
            "\"prevClosePrice\":2749.89," ^
            "\"limitDownPrice\":null," ^
            "\"limitUpPrice\":null," ^
            "\"bankruptLimitDownPrice\":null," ^
            "\"bankruptLimitUpPrice\":null," ^
            "\"prevTotalVolume\":7754701317," ^
            "\"totalVolume\":7761577411," ^
            "\"volume\":6876094," ^
            "\"volume24h\":146032702," ^
            "\"prevTotalTurnover\":607161144999083," ^
            "\"totalTurnover\":607447348615358," ^
            "\"turnover\":286203616275," ^
            "\"turnover24h\":5734209360270," ^
            "\"prevPrice24h\":2707.8," ^
            "\"vwap\":2546.7325," ^
            "\"highPrice\":2798.9," ^
            "\"lowPrice\":2320," ^
            "\"lastPrice\":2418.6," ^
            "\"lastPriceProtected\":2418.6," ^
            "\"lastTickDirection\":\"MinusTick\"," ^
            "\"lastChangePcnt\":-0.1068," ^
            "\"bidPrice\":2418.6," ^
            "\"midPrice\":2418.65," ^
            "\"askPrice\":2418.7," ^
            "\"impactBidPrice\":2418.2042," ^
            "\"impactMidPrice\":2418.95," ^
            "\"impactAskPrice\":2419.6671," ^
            "\"hasLiquidity\":true," ^
            "\"openInterest\":15864085," ^
            "\"openValue\":655012205565," ^
            "\"fairMethod\":\"FundingRate\"," ^
            "\"fairBasisRate\":-2.5513500000000002," ^
            "\"fairBasis\":-2.33," ^
            "\"fairPrice\":2421.95," ^
            "\"markMethod\":\"FairPrice\"," ^
            "\"markPrice\":2421.95," ^
            "\"indicativeTaxRate\":0," ^
            "\"indicativeSettlePrice\":2424.28," ^
            "\"settledPrice\":null," ^
            "\"timestamp\":\"2017-06-15T00:42:10.288Z\"" ^
          "}]," ^
          "\"filter\":{\"symbol\":\"XBTUSD\"}" ^
        "}"
      )
    )
    (InstrumentData {
        table = "instrument";
        keys = Some ["symbol"]
      })

(* websocket_data_of_string *)
let test_websocket_data_of_string_4 test_ctx =
  assert_equal
    (websocket_data_of_string (
      "{" ^
        "\"table\":\"instrument\"," ^
        "\"action\":\"update\"," ^
        "\"data\":[{" ^
            "\"symbol\":\"XBTUSD\"," ^
            "\"totalVolume\":7925736403," ^
            "\"volume\":855766," ^
            "\"totalTurnover\":614593898715392," ^
            "\"turnover\":36199320558," ^
            "\"openInterest\":14722995," ^
            "\"openValue\":624608339880," ^
            "\"timestamp\":\"2017-06-15T22:10:50.856Z\"" ^
        "}]" ^
      "}"
    ))
    (InstrumentDataUpdate {
      table = "instrument";
      action = "update";
      data = [{
        symbol = XBTUSD;
        total_volume = 7925736403;
        volume = 855766;
        total_turnover = 614593898715392;
        turnover = 36199320558;
        open_interest = 14722995;
        open_value = 624608339880;
        timestamp = "2017-06-15T22:10:50.856Z"
      }]
    })

(* websocket_data_of_string *)
let test_websocket_data_of_string_4 test_ctx =
  assert_equal
    (websocket_data_of_string (
      "{" ^
        "\"table\":\"trade\"," ^
        "\"action\":\"insert\"," ^
        "\"data\":[{" ^
          "\"timestamp\":\"2017-06-15T23:12:08.821Z\"," ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"side\":\"Sell\"," ^
          "\"size\":121," ^
          "\"price\":2411," ^
          "\"tickDirection\":\"MinusTick\"," ^
          "\"trdMatchID\":\"be1b1d7a-64b2-03a0-de1e-1d965f8a283c\"," ^
          "\"grossValue\":5018717," ^
           "\"homeNotional\":0.05018717," ^
          "\"foreignNotional\":121" ^
          "}," ^
          "{" ^
          "\"timestamp\":\"2017-06-15T23:12:08.821Z\"," ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"side\":\"Sell\"," ^
          "\"size\":4879," ^
          "\"price\":2410.6," ^
          "\"tickDirection\":\"MinusTick\"," ^
          "\"trdMatchID\":\"381ad59a-5595-b88d-c3ef-cff7f367d863\"," ^
          "\"grossValue\":202395557," ^
          "\"homeNotional\":2.02395557," ^
          "\"foreignNotional\":4879" ^
        "}]" ^
      "}"
    ))
    (TradeData {
      table = "trade";
      action = "insert";
      data = [
        {
          timestamp = "2017-06-15T23:12:08.821Z";
          symbol = XBTUSD;
          side = "Sell";
          size = 121;
          price = 2411.0;
          tick_direction = "MinusTick";
          trd_match_id = "be1b1d7a-64b2-03a0-de1e-1d965f8a283c";
          gross_value = 5018717;
          home_notional = 0.05018717;
          foreign_notional = 121
        };
        {
          timestamp = "2017-06-15T23:12:08.821Z";
          symbol = XBTUSD;
          side = "Sell";
          size = 4879;
          price = 2410.6;
          tick_direction = "MinusTick";
          trd_match_id = "381ad59a-5595-b88d-c3ef-cff7f367d863";
          gross_value = 202395557;
          home_notional = 2.02395557;
          foreign_notional = 4879
        }
      ]
    })

(* websocket_data_of_string *)
let test_websocket_data_of_string_5 test_ctx =
  assert_equal
    (websocket_data_of_string (
      "{" ^
        "\"table\":\"trade\"," ^
        "\"keys\":[]," ^
        "\"types\":{" ^
          "\"timestamp\":\"timestamp\"," ^
          "\"symbol\":\"symbol\"," ^
          "\"side\":\"symbol\"," ^
          "\"size\":\"long\"," ^
          "\"price\":\"float\"," ^
          "\"tickDirection\":\"symbol\"," ^
          "\"trdMatchID\":\"guid\"," ^
          "\"grossValue\":\"long\"," ^
          "\"homeNotional\":\"float\"," ^
          "\"foreignNotional\":\"float\"" ^
        "}," ^
        "\"foreignKeys\":{\"symbol\":\"instrument\",\"side\":\"side\"}," ^
        "\"attributes\":{\"timestamp\":\"sorted\",\"symbol\":\"grouped\"}," ^
        "\"action\":\"partial\"," ^
        "\"data\":[{" ^
          "\"timestamp\":\"2017-06-17T00:49:31.313Z\"," ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"side\":\"Sell\"," ^
          "\"size\":1," ^
          "\"price\":2447.5," ^
          "\"tickDirection\":\"MinusTick\"," ^
          "\"trdMatchID\":\"01adb669-a970-cdf3-549a-b68a31512dce\"," ^
          "\"grossValue\":40858," ^
          "\"homeNotional\":0.00040858," ^
          "\"foreignNotional\":1" ^
        "}]," ^
        "\"filter\":{\"symbol\":\"XBTUSD\"}" ^
      "}"
    ))
    (TradeData {
      table = "trade";
      action = "partial";
      data = [{
        timestamp = "2017-06-17T00:49:31.313Z";
        symbol = XBTUSD;
        side = "Sell";
        size = 1;
        price = 2447.5;
        tick_direction = "MinusTick";
        trd_match_id = "01adb669-a970-cdf3-549a-b68a31512dce";
        gross_value = 40858;
        home_notional = 0.00040858;
        foreign_notional = 1
      }]
    })


(* TODO apply_websocket_feed *)
 (*   {
      "table":"instrument",
              "action":"update",
              "data":[{
                  "symbol":"XBTUSD",
                           "impactBidPrice":2435.401,
                           "impactMidPrice":2435.9,
                           "impactAskPrice":2436.3503,
                           "timestamp":"2017-06-17T01:55:35.000Z"
                }]
      } *)

let test_orderbook_insert_of_json test_ctx =
  assert_equal
    (orderbook_insert_of_json @@ Yojson.Basic.from_string (
        "{" ^
        "\"table\":\"orderBookL2\"," ^
        "\"action\":\"insert\"," ^
        "\"data\":[{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":8792669000," ^
          "\"side\":\"Sell\"," ^
          "\"size\":220," ^
          "\"price\":73310" ^
        "}]" ^
        "}"
      ))
    (OrderbookInsert {
        table = "orderBookL2";
        action = "insert";
        data  = [
          {
            symbol = XBTUSD;
            id = 8792669000;
            side = Sell;
            size = 220;
            price = 73310.0;
          };
        ]
    })

let test_delete test_ctx =
  assert_equal
    (orderbook_delete_of_json @@ Yojson.Basic.from_string (
        "{" ^
        "\"table\":\"orderBookL2\"," ^
        "\"action\":\"delete\"," ^
        "\"data\":[{" ^
          "\"symbol\":\"XBTUSD\"," ^
          "\"id\":8799320700," ^
          "\"side\":\"Buy\"" ^
        "}]" ^
        "}"
      ))
    (OrderbookDelete {
        table = "orderBookL2";
        keys = None;
        action = Some "delete";
        data  = [
          {
            symbol = XBTUSD;
            id = 8799320700;
            side = Buy;
          };
        ]
    })


let suite =
  "suite" >::: [
    "test_0" >:: test_symbol_of_string;
    "test_1" >:: test_info_of_json;
    "test_2" >:: test_subscribe_info_of_json;
    "test_3" >:: test_3;
    "test_5" >:: test_5;
    "test_6" >:: test_6;
    "test_7" >:: test_7;
    "test_8" >:: test_websocket_data_of_string;
    "test_9" >:: test_9;
    "test_10" >:: test_websocket_data_of_string_2;
    "test_11" >:: test_websocket_data_of_string_3;
    "test_12" >:: test_websocket_data_of_string_4;
    "test_13" >:: test_websocket_data_of_string_5;
    "test_delete" >:: test_delete;
    "test_insert" >:: test_orderbook_insert_of_json;
  ]

let _ = run_test_tt_main suite
