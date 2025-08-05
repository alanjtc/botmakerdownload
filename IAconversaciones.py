import csv
import re
from datetime import datetime
from tkinter import Tk, filedialog
import openpyxl
from openpyxl.styles import Alignment, Font
from openpyxl.utils import get_column_letter

from datetime import datetime
from tkinter import Tk, filedialog


SERVICIOS_VALIDOS = [
    'Actualiza Ariba',
    'SBN Órdenes',
    'SBN Facturas',
    'Cert. Tributarios',
    'Facturas por pagar',
    'Aviso de pagos',
    'Relación Comercial',
    'Manual de procesos',
    'Eventos Mercantiles'
]
SOCIEDADES_VALIDAS = [
    'GeoPark',
    'Amerisur'
]

def procesar_conversaciones(archivo_txt, archivo_csv):
    patron = re.compile(r"^(.*?);(.*?);(.*?);(.*?);(.*?);(.*?);(.*?);(.*)$")
    conversaciones = {}
    links_chat = {}

    with open(archivo_txt, encoding="utf-8") as f:
        for linea in f:
            match = patron.match(linea.strip())
            if match:
                chatId, platformContactId, creationTime, sender, msg, extra, extra2, url = match.groups()
                fecha_dia = creationTime[:10]
                clave = f"{chatId}_{fecha_dia}"
                if clave not in conversaciones:
                    conversaciones[clave] = []
                    links_chat[clave] = url if url.startswith("http") else ""
                
                conversaciones[clave].append({
                    "chatId": chatId,
                    "platformContactId": platformContactId,
                    "creationTime": creationTime,
                    "sender": sender,
                    "msg": msg.strip(),
                    "extra": extra.strip(),
                    "extra2": extra2.strip(),
                    "url": url
                })

    filas = []

    # Recorre las conversaciones agrupadas por clave
    for clave, mensajes in conversaciones.items():
        autenticacion = "No"
        bloques = []
        bloque_actual = None
        chatId = mensajes[0]["chatId"]
        ultima_pregunta_servicio = False

        # Si es conversación de prueba, solo agrega una fila y continúa con la siguiente conversación
        if "geoparkbot_test_chat" in mensajes[0]["platformContactId"]:
            dt = datetime.fromisoformat(mensajes[0]["creationTime"].replace("Z", "+00:00"))
            fecha = dt.strftime("%Y-%m-%d")
            hora = dt.strftime("%H:%M:%S")
            filas.append([
                fecha,
                hora,
                chatId,
                mensajes[0]["platformContactId"],
                "",  # NIT
                "",  # Autenticación
                "ConversacionPruebas",
                "",  # Sociedad
                "No Encontrado",
                "",  # Documento
                "",  # Correos
                links_chat.get(clave, "")
            ])
            continue  # Salta el resto del procesamiento para este chat
        
        # Procesa los mensajes de la conversación
#        if "test_chat" in chatId:
#            bloques.append({
#                "servicio": "Conversación de Prueba",
#                "sociedad": "",
#                "resultado": "No Encontrado",
#                "documento": "",
#                "correos": "",
#                "nit": ""
#            })

        i = 0
        sociedad_temp = ""
        
        while i < len(mensajes):
            m = mensajes[i]
            texto_total = f"{m['msg']} {m['extra']} {m['extra2']}".strip().lower()

            if "te damos la bienvenida" in texto_total:
                autenticacion = "Sí"

            # Captura sociedad aunque aún no se haya creado el bloque_actual
            if m["sender"] == "user" and any(s in texto_total for s in ["geopark", "amerisur"]):
                sociedad_temp = m["msg"] or m["extra2"] or m["extra"]
                if bloque_actual:
                    bloque_actual["sociedad"] = sociedad_temp

            if m["sender"] == "user" and re.fullmatch(r"\d{5,}", m["msg"].strip()):
                nit_valor = m["msg"].strip()
                if bloque_actual:
                    bloque_actual["nit"] = nit_valor
                else:
                    bloques.append({
                        "servicio": "",
                        "sociedad": sociedad_temp,
                        "resultado": "No Encontrado",
                        "documento": "",
                        "correos": "",
                        "nit": nit_valor
                    })

            if m["sender"] == "bot" and "¿en cuál de las siguientes tareas te puedo ayudar?" in texto_total:
                ultima_pregunta_servicio = True
                i += 1
                continue

            if ultima_pregunta_servicio and m["sender"] == "user":
                servicio_valor = m["msg"] or m["extra2"] or m["extra"]
                # Validar si el servicio está en la lista de servicios válidos
                if servicio_valor in SERVICIOS_VALIDOS:
                    servicio_final = servicio_valor
                else:
                    servicio_final = ""  # O puedes poner "Servicio no válido" si prefieres

                bloque_actual = {
                    "servicio": servicio_final,
                    "sociedad": sociedad_temp,
                    "resultado": "No Encontrado",
                    "documento": "",
                    "correos": "",
                    "nit": ""
                }
                bloques.append(bloque_actual)
                ultima_pregunta_servicio = False
                i += 1
                continue

            # Si la sociedad se menciona después de escoger servicio, también la asigna
            if bloque_actual and m["sender"] == "user" and any(s in texto_total for s in ["geopark", "amerisur"]):
                bloque_actual["sociedad"] = m["msg"] or m["extra2"] or m["extra"]

            # Buscar documento PDF y extraer correos de la siguiente línea si corresponde
            documento_encontrado = False
            for campo in [m["msg"], m["extra"], m["extra2"], m["url"]]:
                if bloque_actual and ".pdf" in campo.lower():
                    bloque_actual["documento"] = campo
                    bloque_actual["resultado"] = "Encontrado"
                    documento_encontrado = True

            if documento_encontrado and i + 1 < len(mensajes):
                siguiente = mensajes[i + 1]
                texto_sig = f"{siguiente['msg']} {siguiente['extra']} {siguiente['extra2']}".strip()
                if "El PDF fue enviado a los siguientes correos" in texto_sig:
                    texto_correos = texto_sig.replace("-", " ")
                    correos_encontrados = re.findall(r"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+", texto_correos)
                    if correos_encontrados:
                        bloque_actual["correos"] = "; ".join(sorted(set(correos_encontrados)))

            # Si no se llenó correos, buscar en los campos actuales
            if bloque_actual and not bloque_actual["correos"]:
                correos_encontrados = []
                for campo in [m["msg"], m["extra"], m["extra2"]]:
                    if campo:
                        correos_encontrados += re.findall(r"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+", campo)
                if correos_encontrados:
                    bloque_actual["correos"] = "; ".join(sorted(set(correos_encontrados)))
            i += 1

        if not bloques:
            bloques.append({
                "servicio": "Solo Saludo",
                "sociedad": "",
                "resultado": "No Encontrado",
                "documento": "",
                "correos": "",
                "nit": ""
            })

        nit_global = next((b.get("nit", "") for b in bloques if b.get("nit", "")), "")
        if nit_global:
            for b in bloques:
                if not b.get("nit"):
                    b["nit"] = nit_global

        dt = datetime.fromisoformat(mensajes[0]["creationTime"].replace("Z", "+00:00"))
        fecha = dt.strftime("%Y-%m-%d")
        hora = dt.strftime("%H:%M:%S")

        for b in bloques:
            valor_autenticacion = autenticacion
            if b["servicio"] == "Solo Saludo":
                valor_autenticacion = ""
            # Si el resultado es "No Encontrado", no llenar correos enviados
            if b["resultado"] == "No Encontrado":
                b["correos"] = ""

            # Validar que la sociedad esté en el listado de sociedades válidas
            sociedad_valida = b["sociedad"] if b["sociedad"] in SOCIEDADES_VALIDAS else ""

            filas.append([
                fecha,
                hora,
                chatId,
                mensajes[0]["platformContactId"],
                b.get("nit", ""),
                valor_autenticacion,
                b["servicio"],
                sociedad_valida,
                b["resultado"],
                b["documento"],
                b["correos"],
                links_chat.get(clave, "")
            ])

        # Si en algún bloque se generó un PDF, la autenticación será "Sí"
        if any(".pdf" in (b.get("documento") or "").lower() for b in bloques):
            autenticacion = "Sí"

    with open(archivo_csv, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.writer(f, delimiter="|")
        writer.writerow([
            "Fecha", "Hora", "ID Conversación", "Celular Usuario", "NIT Proveedor",
            "¿Autenticación Exitosa?", "Servicio Consultado", "Sociedad", "Resultado",
            "Documento Generado", "Correos Enviados", "Link Conversación"
        ])
        writer.writerows(filas)

def csv_a_xlsx(archivo_csv, archivo_xlsx):
    wb = openpyxl.Workbook()
    ws = wb.active

    # Leer CSV y agregar filas
    with open(archivo_csv, encoding="utf-8-sig") as f:
        for row in f:
            ws.append(row.strip().split("|"))

    # Formatear encabezados
    header_font = Font(bold=True)
    for cell in ws[1]:
        cell.font = header_font
        cell.alignment = Alignment(horizontal="center", vertical="center", wrap_text=True)

    # Ajustar ancho de columnas automáticamente
    for col in ws.columns:
        max_length = 0
        col_letter = get_column_letter(col[0].column)
        for cell in col:
            try:
                if cell.value:
                    max_length = max(max_length, len(str(cell.value)))
            except:
                pass
        ws.column_dimensions[col_letter].width = max_length + 2

    # Alinear el resto de las celdas
    for row in ws.iter_rows(min_row=2):
        for cell in row:
            cell.alignment = Alignment(vertical="top", wrap_text=True)

    wb.save(archivo_xlsx)
    print(f"✅ XLSX generado exitosamente en: {archivo_xlsx}")

if __name__ == "__main__":
    print("🚀 Procesador de conversaciones de bot a CSV")
    root = Tk()
    root.withdraw()
    archivo_txt = filedialog.askopenfilename(title="Selecciona el archivo .txt de entrada", filetypes=[("Archivos de texto", "*.txt")])
    if not archivo_txt:
        print("❌ No se seleccionó archivo de entrada.")
        root.destroy()
        exit()

    archivo_csv = filedialog.asksaveasfilename(title="Guardar archivo CSV", defaultextension=".csv", filetypes=[("Archivo CSV", "*.csv")])
    if not archivo_csv:
        print("❌ No se seleccionó ubicación de salida.")
        root.destroy()
        exit()

    procesar_conversaciones(archivo_txt, archivo_csv)
    print("✅ CSV generado exitosamente en:", archivo_csv)

    archivo_xlsx = filedialog.asksaveasfilename(title="Guardar archivo XLSX", defaultextension=".xlsx", filetypes=[("Archivo XLSX", "*.xlsx")])
    if archivo_xlsx:
        csv_a_xlsx(archivo_csv, archivo_xlsx)

    root.destroy()