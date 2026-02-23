class zcl_itco_bapi_mapper definition
  public
  final
  create public.

public section.

  types: tt_itemdata type table of bapi_incinv_create_item,
         tt_taxdata  type table of bapi_incinv_create_tax,
         tt_return   type table of bapiret2.

  methods create_incoming_invoice
    importing
      nast              type nast
    returning
      value(result)     type bapi_incinv_fld-inv_doc_no
    raising
      cx_bapi_incinv_error
      cx_sy_no_rtti.

  methods get_last_return
    returning
      value(result)     type tt_return.

private section.

  data: mt_return type tt_return.

  methods map_header_data
    importing
      vbrk              type vbrk
      ekko              type ekko
    returning
      value(headerdata) type bapi_incinv_create_header.

  methods map_item_data
    importing
      vbrp              type table of vbrp
      ekpo              type table of ekpo
      konv              type table of konv
      vbrk              type vbrk
    returning
      value(itemdata)   type tt_itemdata.

  methods map_tax_data
    importing
      konv              type table of konv
      ekpo              type table of ekpo
      vbrk              type vbrk
    returning
      value(taxdata)    type tt_taxdata.

  methods read_invoice_data
    importing
      nast              type nast
    exporting
      vbrk              type vbrk
      vbrp              type table of vbrp
      ekko              type ekko
      ekpo              type table of ekpo
      konv              type table of konv
    raising
      cx_bapi_incinv_error.

  methods call_bapi
    importing
      headerdata        type bapi_incinv_create_header
      itemdata          type tt_itemdata
      taxdata           type tt_taxdata
    returning
      value(invoicedocnumber) type bapi_incinv_fld-inv_doc_no
    raising
      cx_bapi_incinv_error.

  methods validate_bapi_response
    importing
      return            type tt_return
    raising
      cx_bapi_incinv_error.

endclass.

class zcl_itco_bapi_mapper implementation.

  method create_incoming_invoice.
    """
    Metodo principal para crear factura de entrada desde NAST
    """
    data: vbrk      type vbrk,
          vbrp      type table of vbrp,
          ekko      type ekko,
          ekpo      type table of ekpo,
          konv      type table of konv,
          headerdata type bapi_incinv_create_header,
          itemdata   type tt_itemdata,
          taxdata    type tt_taxdata.

    try.
        "*-- 1. Validar entrada
        if nast is initial or nast-objky is initial.
          raise exception type cx_bapi_incinv_error
            exporting textid = cx_bapi_incinv_error=>default_textid.
        endif.

        "*-- 2. Lectura de datos
        me->read_invoice_data(
          exporting
            nast = nast
          importing
            vbrk = vbrk
            vbrp = vbrp
            ekko = ekko
            ekpo = ekpo
            konv = konv ).

        "*-- 3. Mapeo de encabezado
        headerdata = me->map_header_data( vbrk = vbrk
                                          ekko = ekko ).

        "*-- 4. Mapeo de items
        itemdata = me->map_item_data( vbrp = vbrp
                                      ekpo = ekpo
                                      konv = konv
                                      vbrk = vbrk ).

        "*-- 5. Mapeo de impuestos
        taxdata = me->map_tax_data( konv = konv
                                    ekpo = ekpo
                                    vbrk = vbrk ).

        "*-- 6. Llamada a BAPI
        result = me->call_bapi( headerdata = headerdata
                                itemdata   = itemdata
                                taxdata    = taxdata ).

        "*-- 7. Validar respuesta
        me->validate_bapi_response( mt_return ).

      catch cx_bapi_incinv_error.
        raise.
    endtry.

  endmethod.

  method read_invoice_data.
    """
    Lee los datos necesarios de factura de venta y orden de compra
    """
    data: lv_purchase_order type ekko-ebeln.

    try.
        "*-- Lectura de cabecera de factura
        select single *
          from vbrk
          where vbeln = @nast-objky
          into @vbrk.

        if sy-subrc <> 0.
          raise exception type cx_bapi_incinv_error.
        endif.

        "*-- Lectura de posiciones de factura
        select *
          from vbrp
          where vbeln = @vbrk-vbeln
          into table @vbrp.

        "*-- Lectura de orden de compra origen
        if vbrp is not initial.
          lv_purchase_order = vbrp[ 1 ]-aubel.

          select single *
            from ekko
            where ebeln = @lv_purchase_order
            into @ekko.

          select *
            from ekpo
            where ebeln = @ekko-ebeln
            into table @ekpo.
        endif.

        "*-- Lectura de tabla de condiciones
        select *
          from konv
          where knumv = @vbrk-knumv
          into table @konv.

      catch cx_sy_table_access_error.
        raise exception type cx_bapi_incinv_error.
    endtry.

  endmethod.

  method map_header_data.
    """
    Mapea los datos del encabezado de la factura a la estructura BAPI
    """
    headerdata-comp_code = ekko-bukrs.
    headerdata-doc_type  = zcl_itco_params=>get_transformation(
                             bukrs      = vbrk-bukrs
                             param_id   = 'DOC_TYPE'
                             field_from = vbrk-fkart ).
    headerdata-doc_date      = vbrk-erdat.
    headerdata-header_txt    = vbrk-vbeln.
    headerdata-currency      = vbrk-waerk.
    headerdata-invoice_ind   = abap_true.
    headerdata-deliv_posting = zcl_itco_params=>get_transformation(
                                 bukrs      = vbrk-bukrs
                                 param_id   = 'DELIV_POSTING'
                                 field_from = vbrk-fkart ).

    "*-- Calculo de importe bruto
    headerdata-gross_amount = reduce #(
      init amount type kwert value 0
      for konv_pos in konv
      where ( kschl in zcl_itco_params=>get_range(
                        bukrs    = vbrk-bukrs
                        param_id = 'AMOUNT_CONDITIONS' ) )
      next amount = amount + konv_pos-kwert ).

  endmethod.

  method map_item_data.
    """
    Mapea las posiciones de factura a items BAPI
    """
    data: ls_item           type bapi_incinv_create_item,
          lv_purchase_order type ekko-ebeln.

    if vbrp is initial.
      return.
    endif.

    lv_purchase_order = vbrp[ 1 ]-aubel.

    loop at vbrp into data(vbrp_pos).
      clear ls_item.

      ls_item-po_number    = lv_purchase_order.
      ls_item-po_item      = vbrp_pos-aupos.
      ls_item-quantity     = vbrp_pos-fkimg.
      ls_item-item_amount  = reduce #(
        init amount type kwert value 0
        for konv_pos in konv
        where ( kschl in zcl_itco_params=>get_range(
                          bukrs    = vbrk-bukrs
                          param_id = 'AMOUNT_CONDITIONS' )
                and kposn = vbrp_pos-posnr )
        next amount = amount + konv_pos-kwert ).

      append ls_item to itemdata.
    endloop.

  endmethod.

  method map_tax_data.
    """
    Mapea los impuestos de la factura a la estructura BAPI
    """
    data: lv_tax_amount type kwert,
          ls_ekpo       type ekpo.

    loop at konv into data(konv_tax)
         where kschl in zcl_itco_params=>get_range(
                        bukrs    = vbrk-bukrs
                        param_id = 'TAX_CONDITIONS' ).

      "*-- Buscar linea de ekpo correspondiente
      try.
          read table ekpo into ls_ekpo with key ebelp = konv_tax-kposn.
      catch cx_sy_itab_line_not_found.
        continue.
      endtry.

      "*-- Calcular monto de impuesto
      lv_tax_amount = reduce #(
        init amount type kwert value 0
        for konv_pos in konv
        where ( kschl = konv_tax-kschl )
        next amount = amount + konv_pos-kwert ).

      append value bapi_incinv_create_tax(
        tax_amount = lv_tax_amount
        tax_code   = ls_ekpo-mwskz
      ) to taxdata.

    endloop.

  endmethod.

  method call_bapi.
    """
    Llama la BAPI para crear la factura de entrada
    """
    data: lv_invoicedocnumber type bapi_incinv_fld-inv_doc_no,
          lv_fiscalyear       type bapi_incinv_fld-fisc_year.

    call function 'BAPI_INCOMINGINVOICE_CREATE'
      exporting
        headerdata       = headerdata
      importing
        invoicedocnumber = lv_invoicedocnumber
        fiscalyear       = lv_fiscalyear
      tables
        itemdata         = itemdata
        taxdata          = taxdata
        return           = mt_return.

    invoicedocnumber = lv_invoicedocnumber.

  endmethod.

  method validate_bapi_response.
    """
    Valida la respuesta de la BAPI
    """
    data: ls_return type bapiret2.

    loop at return into ls_return.
      if ls_return-type = 'E' or ls_return-type = 'A'.
        raise exception type cx_bapi_incinv_error.
      endif.
    endloop.

  endmethod.

  method get_last_return.
    """
    Retorna la tabla de retorno de la ultima ejecucion
    """
    result = mt_return.

  endmethod.

endclass.
