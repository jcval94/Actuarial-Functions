# Arneses

Esta carpeta contiene ejemplos prácticos de **harnesses (arneses)**: capas de orquestación que reúnen entrada, reglas, ejecución, validaciones y salidas alrededor de una tarea.

Un *skill* puede ser una función puntual. Un arnés, en cambio, decide **qué revisar, cómo revisarlo, con qué límites y qué hacer con el resultado**.

## 1. `actuarial_repo_harness`

Arnés específico para este repositorio. Inspecciona los scripts R sin ejecutarlos y genera un diagnóstico reproducible de mantenibilidad y portabilidad.

Revisa, entre otras cosas:

- errores de sintaxis;
- rutas absolutas de Windows;
- uso de `setwd()`, `attach()`, `View()` y `win.graph()`;
- dependencias declaradas con `library()` / `require()`;
- lecturas de archivos con `read.csv()` y similares;
- señales de código interactivo o difícil de automatizar.

## 2. `data_quality_harness`

Arnés reutilizable para validar cualquier CSV antes de usarlo en un análisis.

Calcula:

- dimensiones;
- tipos;
- nulos;
- cardinalidad;
- duplicados;
- resumen numérico;
- reglas de aceptación configurables mediante variables de entorno.

Ambos arneses usan R base para reducir dependencias.
