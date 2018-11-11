module type AccountInfo =
  sig
    val username : string
    val api_key : string
    val secret_key : string
    val data_loc : string option
    val max_lots : int
    val live_trade : bool
  end

type symbol =
  | XBTUSD
  | Other of string

type ticker = {
  symbol : symbol;
  bid : float;
  mid : float;
  ask : float
}

type order_side =
  | Buy
  | Sell

type order_type =
  | Market
  | Limit
  | Stop
  | StopLimit
  | MarketIfTouched
  | LimitIfTouched
  | MarketWithLeftOverAsLimit
  | Pegged

type time_in_force =
  | Day
  | GoodTillCancel
  | ImmediateOrCancel
  | FillOrKill

type execution_instruction =
  | ParticipateDoNotInitiate
  | AllOrNone
  | MarkPrice
  | IndexPrice
  | LastPrice
  | Close
  | ReduceOnly
  | Fixed

type contingency_type =
  | OneCancelsTheOther
  | OneTriggersTheOther
  | OneUpdatesTheOtherAbsolute
  | OneUpdatesTheOtherProportional

type new_order = {
  symbol : symbol;
  side : order_side;
  simple_order_qty : float option; (* Order qty in units of underlying instrument (i.e. Bitcoin). *)
  order_qty : int option; (*Order quantity in units of the instrument (i.e. contracts). *)
  price : float option;
  display_qty : float option; (* Hidden orders incur the taker fee *)
  stop_px : float option;
  cl_ord_id : string option;
  cl_ord_link_id : string option;
  peg_offset_value : float option;
  peg_price_type : string option;
  ord_type : order_type option;
  time_in_force : time_in_force option;
  exec_inst : execution_instruction option;
  contingency_type : contingency_type option;
  text_string : string option
}

(* Send to amend an order *)
type amend_order = {
  order_id : string;
  orig_cl_ord_id : string option;
  cl_ord_id : string option;
  simple_order_qty : float option;
  order_qty : float option;
  simple_leaves_qty : float option;
  leaves_qty : float option;
  price : float option;
  stop_px : float option;
  peg_offset_value : float option;
  text : float option
}

(* API return after placing a new order *)
type order_return = {
  order_id : string;
  cl_ord_id : string option;
  cl_ord_link_id : string option;
  account : float option;
  symbol : symbol;
  side : order_side option;
  simple_order_qty : float option;
  order_qty : float option;
  price : float option;
  display_qty :float option;
  stop_px : float option;
  peg_offset_value : float option;
  peg_price_type : string option;
  currency : string option;
  settl_currency : string option;
  ord_type : string option;
  time_in_force : string option;
  exec_inst : string option;
  contingency_type :string option;
  ex_destination : string option;
  ord_status : string option;
  triggered : string option;
  working_indicator : bool option;
  ord_rej_reason : string option;
  simple_leaves_qty : float option;
  leaves_qty : float option;
  simple_cum_qty : float option;
  cum_qty : float option;
  avg_px : float option;
  multi_leg_reporting_type : string option;
  text : string option;
  transact_time : string option; (*	??? docs say datetime [optional] *)
  timestamp : string option	(* ??? docs say datetime [optional] *)
}

type order_return_error_msg = {
  message : string;
  name : string
}

type order_return_error = {error : order_return_error_msg}

type order_receipt = OrderReturn of order_return | OrderReturnError of order_return_error

type position_return = {
  account : int;
  symbol : symbol;
  currency : string;
  underlying : string option;
  quote_currency : string option;
  commission : float option;
  init_margin_req : float option;
  maint_margin_req : float option;
  risk_limit : float option;
  leverage : float option;
  cross_margin : bool option;
  deleverage_percentile : float option;
  rebalanced_pnl : float option;
  prev_realised_pnl : float option;
  prev_unrealised_pnl : float option;
  prev_close_price : float option;
  opening_timestamp : string option;
  opening_qty : float option;
  opening_cost : float option;
  opening_comm : float option;
  open_order_buy_qty : float option;
  open_order_buy_cost : float option;
  open_order_buy_premium : float option;
  open_order_sell_qty : float option;
  open_order_sell_cost : float option;
  open_order_sell_premium : float option;
  exec_buy_qty : float option;
  exec_buy_cost : float option;
  exec_sell_qty : float option;
  exec_sell_cost : float option;
  exec_qty : float option; (**)
  exec_cost : float option;
  exec_comm : float option;
  current_timestamp : string option;
  current_qty : float option;
  current_cost : float option;
  current_comm : float option;
  realised_cost : float option;
  unrealised_cost : float option;
  gross_open_cost : float option;
  gross_open_premium : float option;
  gross_exec_cost : float option;
  is_open : bool option;
  mark_price : float option;
  mark_value : float option;
  risk_value : float option;
  home_notional : float option;
  foreign_notional : float option;
  pos_state : string option;
  pos_cost : float option;
  pos_cost2 : float option;
  pos_cross : float option;
  pos_init : float option;
  pos_comm : float option;
  pos_loss : float option;
  pos_margin : float option;
  pos_maint : float option;
  pos_allowance : float option;
  taxable_margin : float option;
  init_margin : float option;
  maint_margin : float option;
  session_margin : float option;
  target_excess_margin : float option;
  var_margin : float option;
  realised_gross_pnl : float option;
  realised_tax : float option;
  realised_pnl : float option;
  unrealised_gross_pnl : float option;
  long_bankrupt : float option;
  short_bankrupt : float option;
  tax_base : float option;
  indicative_tax_rate : float option;
  indicative_tax : float option;
  unrealised_tax : float option;
  unrealised_pnl : float option;
  unrealised_pnl_pcnt : float option;
  unrealised_roe_pcnt : float option;
  simple_qty : float option;
  simple_cost : float option;
  simple_value : float option;
  simple_pnl : float option;
  simple_pnl_pcnt : float option;
  avg_cost_price : float option;
  avg_entry_price : float option;
  break_even_price : float option;
  margin_call_price : float option;
  liquidation_price : float option;
  bankrupt_price : float option;
  timestamp : string option;
  last_price : float option;
  last_value : float option;
}

type position_receipt = PositionReturn of position_return | PositionReturnError of string
