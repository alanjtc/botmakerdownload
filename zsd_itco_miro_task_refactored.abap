function zsd_itco_miro_task.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NAST) TYPE  NAST
*"----------------------------------------------------------------------

  data: mapper           type ref to zcl_itco_bapi_mapper,
        invoicedocnumber type bapi_incinv_fld-inv_doc_no,
        lv_message       type string.

  try.
      "*-- Instanciar mapper
      mapper = new zcl_itco_bapi_mapper( ).

      "*-- Crear factura de entrada
      invoicedocnumber = mapper->create_incoming_invoice( nast ).

      "*-- Confirmar transaccion
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      "*-- Mensaje de exito
      lv_message = |Factura {invoicedocnumber} creada exitosamente|.
      message lv_message type 'S'.

    catch cx_bapi_incinv_error into data(exc).
      message 'Error al crear factura de entrada' type 'E'.
      return.

    catch cx_system_error into data(sys_exc).
      message 'Error de sistema' type 'E'.
      return.

  endtry.

endfunction.
