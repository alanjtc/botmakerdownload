function zsd_itco_miro_task.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NAST) TYPE  NAST
*"----------------------------------------------------------------------

  data:
    headerdata       type bapi_incinv_create_header,
    addressdata      type bapi_incinv_create_addressdata,
    invoicedocnumber type bapi_incinv_fld-inv_doc_no,
    fiscalyear       type table of  bapi_incinv_fld-fisc_year,
    itemdata         type table of bapi_incinv_create_item,
    item             type bapi_incinv_create_item,
    taxdata          type table of bapi_incinv_create_tax,
    return           type table of bapiret2.

  sy-subrc = 4.

  while sy-subrc <> 0.

*-  Obtenemos la cabecera de la factura
    select single *
      from vbrk
      where vbeln = @nast-objky
      into @data(vbrk).

  endwhile.

*- Obtenemos las posiciones de la factura
  select *
    from vbrp
    where vbeln = @vbrk-vbeln
    into table @data(vbrp).

*- Obtenemos la cabecera de la orden de compra
  data(purchase_order) = vbrp[ 1 ]-aubel.

  select single *
    from ekko
    where ebeln = @purchase_order
    into @data(ekko).

*- Obtenemos las posiciones de la orden de compra
  select *
    from ekpo
    where ebeln = @ekko-ebeln
    into table @data(ekpo).

*- Obtenemos la tabla de condiciones
  select *
    from konv
    where knumv = @vbrk-knumv
    into table @data(konv).

*- Mapeamos la información pertinente
  headerdata-comp_code = ekko-bukrs.
  headerdata-doc_type  = zcl_itco_params=>get_transformation( bukrs      = vbrk-bukrs
                                                             param_id   = 'DOC_TYPE'
                                                             field_from = vbrk-fkart ).
  headerdata-doc_date      = vbrk-erdat.

  headerdata-ref_doc_no    =
  headerdata-header_txt    = vbrk-vbeln.
  headerdata-currency      = vbrk-waerk.
  headerdata-gross_amount  = reduce #( init amount type kwert
                                        for konv_pos in konv where ( kschl in zcl_itco_params=>get_range( bukrs    = vbrk-bukrs
                                                                                                          param_id = 'AMOUNT_CONDITIONS' ) )
                                       next amount = amount + konv_pos-kwert ).
  headerdata-diff_inv      = vbrk-bukrs.

  headerdata-invoice_ind   = abap_true.
  headerdata-deliv_posting = zcl_itco_params=>get_transformation( bukrs      = vbrk-bukrs
                                                               param_id   = 'DELIV_POSTING'
                                                               field_from = vbrk-fkart ).


  loop at vbrp into data(vbrp_pos).

    item-po_number = purchase_order.
    item-quantity = vbrp_pos-fkimg.
    item-item_amount = reduce #( init amount type kwert
                                        for konv_pos in konv where ( kschl in zcl_itco_params=>get_range( bukrs    = vbrk-bukrs
                                                                                                          param_id = 'AMOUNT_CONDITIONS' )
                                                                     and kposn = vbrp_pos-posnr )
                                       next amount = amount + konv_pos-kwert ).
    clear:
    item.
  endloop.


  loop at konv into data(konv_tax) where kschl in zcl_itco_params=>get_range( bukrs    = vbrk-bukrs
                                                                              param_id = 'TAX_CONDITIONS' ).

    append value #( tax_amount = reduce #( init amount type kwert
                                            for konv_pos in konv where ( kschl = konv_tax-kschl )
                                           next amount = amount + konv_pos-kwert )
                    tax_code   = ekpo[ ebelp = konv_tax-kposn ]
     ) to  taxdata.

  endloop.

*- Llenamos la información de la BAPI
  call function 'BAPI_INCOMINGINVOICE_CREATE'
    exporting
      headerdata       = headerdata
    importing
      invoicedocnumber = invoicedocnumber
      fiscalyear       = fiscalyear
    tables
      itemdata         = itemdata
      taxdata          = taxdata
      return           = return.

  clear:
  headerdata,
  addressdata,
  invoicedocnumber,
  fiscalyear,
  itemdata,
  taxdata,
  return.


endfunction.