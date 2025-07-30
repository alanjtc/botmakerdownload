import requests
import os
import time
from datetime import date, timedelta
 
API_TOKEN = "eyJhbGciOiJIUzUxMiJ9.eyJidXNpbmVzc0lkIjoiZ2VvcGFya2JvdCIsIm5hbWUiOiJNYXVyaWNpbyBQYWNoZWNvIiwiYXBpIjp0cnVlLCJpZCI6InRZZk9jM0x4cmhWdUtCN1JzdlVjdG9USTkzRzMiLCJleHAiOjE5MTA4MDYxMTUsImp0aSI6InRZZk9jM0x4cmhWdUtCN1JzdlVjdG9USTkzRzMifQ.Mxivb609QH73O4oF3xVkIzbn1LIy8LUdJKAfR1Zh3wZ2cZUZwQmgx6f4x-D19x-dN-C7rUamlkfp0e-vXIk_Eg"
HEADERS = {
    "access-token": API_TOKEN
}
 
SESSIONS_URL = "https://api.botmaker.com/v2.0/sessions"
MESSAGES_URL = "https://api.botmaker.com/v2.0/messages"
 
# Solicitar fechas al usuario
fecha_inicio_str = input("Ingrese la fecha de inicio (YYYY-MM-DD): ")
fecha_fin_str = input("Ingrese la fecha de fin (YYYY-MM-DD): ")

fecha_inicio = date.fromisoformat(fecha_inicio_str)
fecha_fin = date.fromisoformat(fecha_fin_str)

OUTPUT_DIR = f"sessions_{fecha_inicio}"
os.makedirs(OUTPUT_DIR, exist_ok=True)


def down_sessions_from_month(fecha_inicio, fecha_fin):
    # Solo una consulta al API con el rango de fechas
    params = {
        "from": fecha_inicio,
        "to": fecha_fin + timedelta(days=1),
        "include-events": "false",
        "limit": 1000,  # Ajusta el límite según lo que permita la API
        "offset": 0
    }

    response = requests.get(SESSIONS_URL, headers={"access-token": API_TOKEN}, params=params)

    if response.status_code != 200:
        print(f"❌ Error al obtener sesiones: {response.status_code}")
        return

    data = response.json()
    items = data.get("items", [])

    session_ids = []
    print(f"🔎 Se encontraron {len(items)} sesiones en el mes.")
    for session in items:
        session_id = session.get("chat")
        if session_id:
            session_ids.append((session_id.get("chat").get("chatId"),
                                session_id.get("chat").get("contactId")))

    # Crear un solo archivo para todas las sesiones del mes
    filename = f"{OUTPUT_DIR}/{fecha_inicio}_to_{fecha_fin}_all_sessions.txt"

    with open(filename, "w", encoding="utf-8") as f:
        for idx, (session_id, contactId) in enumerate(session_ids):
            url = f"{MESSAGES_URL}?chat-id={session_id}&from={fecha_inicio}T00:00:00Z&to={fecha_fin}T23:59:59Z&long-term-search=true"
            response = requests.get(url, headers=HEADERS)

            if response.status_code != 200:
                print(f"❌ Error en mensajes para sesión {session_id}: {response.status_code}")
                continue

            data = response.json()
            messages = data.get("items", [])

            for msg in messages:
                timestamp = msg.get("creationTime", "")
                remitente = msg.get("from", "")
                texto = msg.get("content").get("text", "").replace("\n", " ").strip()
                buttons = msg.get("content").get("buttons", '')
                selectedButton = msg.get("content").get("selectedButton", '')
                media = msg.get("content").get("media")
                url_media = ""
                if media:
                    url_media = media.get("url", "")

                f.write(f"{session_id};{contactId};{timestamp};{remitente};{texto};{buttons};{selectedButton};{url_media}\n")
            porcentaje = ((idx + 1) / len(session_ids)) * 100
            print(f"Sesion {session_id} escrita en el archivo. Avance: {porcentaje:.2f}% ")

    print(f"✔ Guardado: {filename}")
    print("✅ Todas las conversaciones del mes han sido exportadas.")

# Llama la función para el mes completo
start_time = time.time()  # Inicia el cronómetro
down_sessions_from_month(fecha_inicio, fecha_fin)
end_time = time.time()  # Finaliza el cronómetro
elapsed = end_time - start_time
print(f"⏱ Tiempo de ejecución: {elapsed:.2f} segundos.")