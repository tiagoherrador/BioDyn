from __future__ import annotations
"""GeoBio 2.0 core utilities

Transforming biological complexity into clear, data-driven insight.
Author: Tiago Alejo Herrador | TH BioData Consulting <thbiodata@gmail.com>
Version: 2.0.6
"""

# ----------------------- Imports (auto-collected) -----------------------
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from statsmodels.nonparametric.smoothers_lowess import lowess
import geopandas as gpd
import shapely
from shapely.ops import unary_union
from sklearn.neighbors import BallTree
from shapely.prepared import prep
import os
from typing import Dict, Union, IO
from shapely.geometry import Point
import rasterio
import folium
from folium.plugins import Draw
import plotly.express as px
from matplotlib import gridspec

def load_data(path: str) -> pd.DataFrame:
    """Carga un CSV en un DataFrame."""
    return pd.read_csv(path)

def build_presence_from_checklists(sightings_path: str, checklist_path: str, *, key_col: str='SAMPLING EVENT IDENTIFIER', presence_col: str='Presencia', positive_label: str='Positiva', negative_label: str='Negativa', sep: str='\t', dtype_key=str, usecols_sightings: list | None=None, low_memory: bool=False) -> pd.DataFrame:
    """
    Construye una columna de presencia/ausencia a nivel checklist a partir de:
      - 'avistajes' (sightings/observations): eventos con especies detectadas
      - 'checklist' (sampling): todos los eventos de muestreo

    Flujo:
      1) Lee ambos archivos (TSV por defecto).
      2) Fuerza el tipo del identificador `key_col` a `dtype_key` en ambos.
      3) Crea columna `presence_col` con `positive_label` en avistajes.
      4) Merge LEFT desde checklist a avistajes (para no perder muestreos sin avistajes).
      5) Rellena NaN como `negative_label`.

    Parámetros:
      sightings_path: ruta al archivo de avistajes (eBird “EBD”).
      checklist_path: ruta al archivo de checklists (eBird “Sampling”).
      key_col: nombre de la columna que identifica el evento de muestreo.
      presence_col: nombre de la columna de salida para presencia/ausencia.
      positive_label / negative_label: etiquetas a usar.
      sep: separador del archivo (por defecto tabulador).
      dtype_key: tipo a forzar para `key_col` (por defecto str).
      usecols_sightings: columnas a leer de avistajes; si None, lee todas.
      low_memory: pasa a `read_csv` para optimizar memoria.

    Retorna:
      DataFrame con todas las filas de checklist y la columna `presence_col`.
    """
    avistajes = pd.read_csv(sightings_path, sep=sep, low_memory=low_memory, usecols=usecols_sightings)
    checklist = pd.read_csv(checklist_path, sep=sep, low_memory=low_memory)
    avistajes[key_col] = avistajes[key_col].astype(dtype_key)
    checklist[key_col] = checklist[key_col].astype(dtype_key)
    avistajes = avistajes.copy()
    avistajes[presence_col] = positive_label
    avistajes_min = avistajes[[key_col, presence_col]].drop_duplicates()
    merged = checklist.merge(avistajes_min, on=key_col, how='left')
    merged[presence_col] = merged[presence_col].fillna(negative_label)
    return merged

def filter_data(df: pd.DataFrame, max_distance_km: float=5, max_observers: int=10, max_duration_min: int=300, allowed_protocols: list=['Stationary', 'Traveling'], require_all_species: bool=True, drop_duplicates: bool=True) -> pd.DataFrame:
    """
    Filtra un DataFrame de eBird según criterios de esfuerzo y calidad.

    Parámetros:
    -----------
    df : pd.DataFrame
        DataFrame con las columnas 'EFFORT DISTANCE KM', 'NUMBER OBSERVERS',
        'DURATION MINUTES', 'PROTOCOL NAME' y 'ALL SPECIES REPORTED'.
    max_distance_km : float
        Distancia máxima (en km) permitida en 'EFFORT DISTANCE KM'.
    max_observers : int
        Máximo número de observadores permitidos.
    max_duration_min : int
        Duración máxima (en minutos) permitida.
    allowed_protocols : list
        Lista de protocolos válidos (ej. ['Stationary', 'Traveling']).
    require_all_species : bool
        Si True, mantiene solo filas donde 'ALL SPECIES REPORTED' == 1.
    drop_duplicates : bool
        Si True, elimina duplicados basándose en 'OBSERVATION DATE' y 'LATITUDE'.

    Retorna:
    --------
    pd.DataFrame
        DataFrame filtrado según los criterios establecidos.
    """
    df_filtered = df.copy()
    df_filtered = df_filtered[df_filtered['EFFORT DISTANCE KM'] <= max_distance_km]
    df_filtered = df_filtered[df_filtered['NUMBER OBSERVERS'] <= max_observers]
    df_filtered = df_filtered[df_filtered['DURATION MINUTES'] <= max_duration_min]
    df_filtered = df_filtered[df_filtered['PROTOCOL NAME'].isin(allowed_protocols)]
    if require_all_species:
        df_filtered = df_filtered[df_filtered['ALL SPECIES REPORTED'] == 1]
    if drop_duplicates:
        df_filtered = df_filtered.drop_duplicates(subset=['OBSERVATION DATE', 'LATITUDE'], keep='first').reset_index(drop=True)
    return df_filtered

def _pick_metric_epsg_from_pts(lon, lat):
    lonc = float(np.nanmedian(lon))
    latc = float(np.nanmedian(lat))
    zone = int((lonc + 180) // 6) + 1
    south = latc < 0
    return 32700 + zone if south else 32600 + zone

def plot_checklists_radio_static(merged: pd.DataFrame, *, lat_col: str='LATITUDE', lon_col: str='LONGITUDE', presencia_col: str='Presencia', etiqueta_presencia_positiva: str='Positiva', umbral_km: float=100.0, distancia_col: str='distancia_min_km', base_gdf: gpd.GeoDataFrame | None=None, show_union_buffer: bool=True, max_buffers: int=500, titulo: str='Presencias (azul) y ausencias (rojo) con umbral por radio', figsize=(9, 9), color_presencia: str='royalblue', color_ausencia_kept: str='tomato', color_ausencia_out: str='lightgray', color_buffer: str='black', alpha_pts: float=0.85, savepath: str | None=None) -> pd.DataFrame:
    """
    Visualiza espacialmente presencias y ausencias en función de un umbral de distancia,
    y devuelve un DataFrame con los registros considerados “válidos” (presencias y ausencias
    dentro del radio establecido).

    ---------------------------------------------------
    🔍 DESCRIPCIÓN GENERAL
    ---------------------------------------------------
    Esta función permite inspeccionar espacialmente los puntos de muestreo (por ejemplo,
    listas de eBird) según su distancia a los registros de presencia. Para cada punto de
    ausencia, se calcula la distancia al registro positivo más cercano y se clasifica
    como “dentro” o “fuera” del umbral (`umbral_km`).

    Luego:
      - Dibuja presencias (en azul).
      - Dibuja ausencias dentro del radio (en rojo).
      - Dibuja ausencias fuera del radio (en gris).
      - (Opcional) Traza el contorno de la unión de buffers de radio alrededor de los positivos.
      - (Opcional) Superpone una capa base (ej. límites de ecorregión o país).

    ---------------------------------------------------
    ⚙️ PARÁMETROS
    ---------------------------------------------------
    merged : pd.DataFrame
        DataFrame con coordenadas y la columna de presencia/ausencia.
    lat_col, lon_col : str
        Nombres de las columnas con latitud y longitud.
    presencia_col : str
        Nombre de la columna que indica presencia o ausencia.
    etiqueta_presencia_positiva : str
        Valor que identifica las presencias (por defecto "Positiva").
    umbral_km : float
        Distancia máxima (en km) considerada para mantener ausencias cercanas.
    distancia_col : str
        Columna donde se almacenan (o calculan) las distancias mínimas a presencias.
    base_gdf : gpd.GeoDataFrame, opcional
        Capa geográfica base (por ejemplo límites de país o ecorregión).
    show_union_buffer : bool
        Si True, dibuja la unión del área cubierta por los buffers de radio.
    max_buffers : int
        Límite de puntos positivos a procesar por lote (optimiza memoria).
    titulo : str
        Título del gráfico.
    figsize : tuple
        Tamaño del gráfico en pulgadas.
    color_presencia, color_ausencia_kept, color_ausencia_out, color_buffer : str
        Colores de los puntos y contorno.
    alpha_pts : float
        Transparencia de los puntos.
    savepath : str, opcional
        Si se indica, guarda el mapa en la ruta especificada (formato PNG).

    ---------------------------------------------------
    📤 SALIDA
    ---------------------------------------------------
    pd.DataFrame:
        Retorna un DataFrame con las siguientes columnas:
        - Todas las columnas originales.
        - 'distancia_min_km': distancia al positivo más cercano.
        - 'within_umbral': True si el punto está dentro del umbral o es una presencia.

        En otras palabras:
          ✅ Presencias siempre incluidas.
          ✅ Ausencias dentro del umbral incluidas.
          🚫 Ausencias fuera del umbral excluidas.

    ---------------------------------------------------
    🧭 USO TÍPICO
    ---------------------------------------------------
    >>> df_filtrado = plot_checklists_radio_static(
    ...     merged=datos,
    ...     umbral_km=50,
    ...     base_gdf=ecorregion_gdf
    ... )
    >>> display(df_filtrado.head())

    El gráfico mostrará la distribución espacial, y `df_filtrado`
    contendrá solo los puntos considerados “dentro del radio”.
    """
    for c in (lat_col, lon_col, presencia_col):
        if c not in merged.columns:
            raise ValueError(f"Falta la columna '{c}' en merged.")
    df = merged.copy()
    df[lat_col] = pd.to_numeric(df[lat_col], errors='coerce')
    df[lon_col] = pd.to_numeric(df[lon_col], errors='coerce')
    df = df.dropna(subset=[lat_col, lon_col]).reset_index(drop=True)
    if distancia_col not in df.columns:
        pos = df[df[presencia_col].astype(str) == str(etiqueta_presencia_positiva)].copy()
        neg = df[df[presencia_col].astype(str) != str(etiqueta_presencia_positiva)].copy()
        if len(pos) == 0:
            df[distancia_col] = np.inf
        else:
            R = 6371.0
            pos_rad = np.radians(pos[[lat_col, lon_col]].to_numpy())
            neg_rad = np.radians(neg[[lat_col, lon_col]].to_numpy())
            tree = BallTree(pos_rad, metric='haversine')
            dist_rad, _ = tree.query(neg_rad, k=1)
            dist_km = dist_rad[:, 0] * R
            df.loc[neg.index, distancia_col] = dist_km
            df.loc[pos.index, distancia_col] = 0.0
    es_pos = df[presencia_col].astype(str) == str(etiqueta_presencia_positiva)
    es_neg = ~es_pos
    neg_kept = es_neg & (df[distancia_col] < umbral_km)
    neg_out = es_neg & (df[distancia_col] >= umbral_km)
    df['within_umbral'] = False
    df.loc[es_pos | neg_kept, 'within_umbral'] = True
    gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df[lon_col], df[lat_col]), crs='EPSG:4326')
    gdf_pos = gdf.loc[es_pos]
    gdf_neg_kept = gdf.loc[neg_kept]
    gdf_neg_out = gdf.loc[neg_out]
    union_buffer_wgs84 = None
    if show_union_buffer and len(gdf_pos) > 0 and (umbral_km > 0):
        epsg_metric = _pick_metric_epsg_from_pts(gdf[lon_col].values, gdf[lat_col].values)
        pos_metric = gdf_pos.to_crs(epsg_metric)
        geoms = list(pos_metric.geometry.values)
        if len(geoms) > max_buffers:
            chunk = int(np.ceil(len(geoms) / np.ceil(len(geoms) / max_buffers)))
            unions = []
            for i in range(0, len(geoms), chunk):
                part = gpd.GeoSeries(geoms[i:i + chunk], crs=epsg_metric).buffer(umbral_km * 1000.0)
                unions.append(unary_union(part))
            union_metric = unary_union(unions)
        else:
            union_metric = unary_union(gpd.GeoSeries(geoms, crs=epsg_metric).buffer(umbral_km * 1000.0))
        union_buffer_wgs84 = gpd.GeoSeries([union_metric], crs=epsg_metric).to_crs(4326)
    fig, ax = plt.subplots(figsize=figsize)
    if base_gdf is not None and len(base_gdf) > 0:
        base_gdf.to_crs(4326).boundary.plot(ax=ax, linewidth=1.0, color='black', alpha=0.6)
    if union_buffer_wgs84 is not None:
        union_buffer_wgs84.boundary.plot(ax=ax, color=color_buffer, linewidth=1.2, label=f'Umbral {umbral_km} km')
    if len(gdf_neg_out) > 0:
        gdf_neg_out.plot(ax=ax, markersize=6, color=color_ausencia_out, alpha=0.35, label='Ausencias (fuera)')
    if len(gdf_neg_kept) > 0:
        gdf_neg_kept.plot(ax=ax, markersize=10, color=color_ausencia_kept, alpha=alpha_pts, label='Ausencias (dentro)')
    if len(gdf_pos) > 0:
        gdf_pos.plot(ax=ax, markersize=12, color=color_presencia, alpha=alpha_pts, label='Presencias')
    ax.set_xlabel('Longitud')
    ax.set_ylabel('Latitud')
    ax.set_title(titulo)
    ax.grid(True, linestyle='--', linewidth=0.4, alpha=0.5)
    ax.legend(loc='best')
    try:
        ax.set_aspect('equal', adjustable='datalim')
    except Exception:
        pass
    if savepath:
        plt.savefig(savepath, dpi=300, bbox_inches='tight')
    plt.show()
    df_filtrado = df.loc[es_pos | neg_kept].copy().reset_index(drop=True)
    return df_filtrado

def filtrar_checklists_por_region_fast(merged: pd.DataFrame, region_path: str | None=None, region_gdf: gpd.GeoDataFrame | None=None, lat_col: str='LATITUDE', lon_col: str='LONGITUDE', presencia_col: str='Presencia', etiqueta_presencia_positiva: str='Positiva', keep_positivas: bool=True, dentro: bool=True, buffer_km: float=0.0, eco_field: str | None=None, eco_values: list[str] | None=None, simplify_tolerance_m: float | None=None):
    df = merged.copy()
    for c in (lat_col, lon_col):
        if c not in df.columns:
            raise ValueError(f"Falta la columna '{c}'.")
    df[lat_col] = pd.to_numeric(df[lat_col], errors='coerce')
    df[lon_col] = pd.to_numeric(df[lon_col], errors='coerce')
    df = df.dropna(subset=[lat_col, lon_col]).reset_index(drop=True)
    if region_gdf is None:
        eco = gpd.read_file(region_path)
    else:
        eco = region_gdf.copy()
    if eco.crs is None:
        eco = eco.set_crs(4326, allow_override=True)
    else:
        eco = eco.to_crs(4326)
    if eco_field and eco_values:
        eco = eco[eco[eco_field].astype(str).isin([str(v) for v in eco_values])]
        if eco.empty:
            raise ValueError('La selección eco_field + eco_values no devolvió geometrías.')
    eco_union = unary_union(eco.geometry)
    if buffer_km and buffer_km != 0:
        lonc, latc = (eco_union.centroid.xy[0][0], eco_union.centroid.xy[1][0])
        zone = int((lonc + 180) // 6) + 1
        south = latc < 0
        metric_epsg = 32700 + zone if south else 32600 + zone
        eco_union = gpd.GeoSeries([eco_union], crs=4326).to_crs(metric_epsg).buffer(buffer_km * 1000).to_crs(4326).iloc[0]
    if simplify_tolerance_m and simplify_tolerance_m > 0:
        lonc, latc = (eco_union.centroid.xy[0][0], eco_union.centroid.xy[1][0])
        zone = int((lonc + 180) // 6) + 1
        south = latc < 0
        metric_epsg = 32700 + zone if south else 32600 + zone
        eco_union_metric = gpd.GeoSeries([eco_union], crs=4326).to_crs(metric_epsg).iloc[0]
        eco_union_metric = shapely.simplify(eco_union_metric, simplify_tolerance_m, preserve_topology=True)
        eco_union = gpd.GeoSeries([eco_union_metric], crs=metric_epsg).to_crs(4326).iloc[0]
    gdfp = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df[lon_col], df[lat_col]), crs=4326)
    minx, miny, maxx, maxy = eco_union.bounds
    lon = gdfp[lon_col].to_numpy()
    lat = gdfp[lat_col].to_numpy()
    mask_bbox = (lon >= minx) & (lon <= maxx) & (lat >= miny) & (lat <= maxy)
    mask_preciso = np.zeros(len(gdfp), dtype=bool)
    if mask_bbox.any():
        pts_candidatos = gdfp.geometry.values[mask_bbox]
        prep_poly = prep(eco_union)
        mask_preciso_bbox = np.fromiter((prep_poly.covers(p) for p in pts_candidatos), count=mask_bbox.sum(), dtype=bool)
        mask_preciso[mask_bbox] = mask_preciso_bbox
    mask_region = mask_preciso if dentro else ~mask_preciso
    if keep_positivas and presencia_col in gdfp.columns:
        es_pos = gdfp[presencia_col].astype(str).values == str(etiqueta_presencia_positiva)
        mask_final = mask_region | es_pos
    else:
        mask_final = mask_region
    out = gdfp.loc[mask_final].drop(columns='geometry').reset_index(drop=True)
    resumen = {'total': len(gdfp), 'candidatos_bbox': int(mask_bbox.sum()), 'dentro_exactos': int(mask_preciso.sum()), 'kept': len(out), 'simplify_tolerance_m': simplify_tolerance_m or 0, 'buffer_km': buffer_km or 0}
    return (out, resumen)

def _pick_metric_epsg_from_geom(geom):
    lonc, latc = (geom.centroid.xy[0][0], geom.centroid.xy[1][0])
    zone = int((lonc + 180) // 6) + 1
    south = latc < 0
    return 32700 + zone if south else 32600 + zone

def _load_region_union(region_path: str | None=None, region_gdf: gpd.GeoDataFrame | None=None, eco_field: str | None=None, eco_values: list[str] | None=None, buffer_km: float=0.0, simplify_tolerance_m: float | None=None):
    """Carga la capa de ecorregión y devuelve (geom disuelta, GeoDF WGS84)."""
    if region_gdf is not None:
        eco = region_gdf.copy()
    else:
        eco = gpd.read_file(region_path)
    eco = eco.set_crs(4326, allow_override=True) if eco.crs is None else eco.to_crs(4326)
    if eco_field and eco_values:
        eco = eco[eco[eco_field].astype(str).isin([str(v) for v in eco_values])]
        if eco.empty:
            raise ValueError('La selección eco_field + eco_values no devolvió geometrías.')
    eco_union = unary_union(eco.geometry)
    if buffer_km and buffer_km != 0:
        metric_epsg = _pick_metric_epsg_from_geom(gpd.GeoSeries([eco_union], crs=4326).iloc[0])
        eco_union = gpd.GeoSeries([eco_union], crs=4326).to_crs(metric_epsg).buffer(buffer_km * 1000).to_crs(4326).iloc[0]
    if simplify_tolerance_m and simplify_tolerance_m > 0:
        metric_epsg = _pick_metric_epsg_from_geom(gpd.GeoSeries([eco_union], crs=4326).iloc[0])
        eco_union_metric = gpd.GeoSeries([eco_union], crs=4326).to_crs(metric_epsg).iloc[0]
        eco_union_metric = shapely.simplify(eco_union_metric, simplify_tolerance_m, preserve_topology=True)
        eco_union = gpd.GeoSeries([eco_union_metric], crs=metric_epsg).to_crs(4326).iloc[0]
    return (eco_union, eco.to_crs(4326))

def plot_region_checklists_from_fast(df_filtrado: pd.DataFrame, *, region_path: str | None=None, region_gdf: gpd.GeoDataFrame | None=None, eco_field: str | None=None, eco_values: list[str] | None=None, buffer_km: float=0.0, simplify_tolerance_m: float | None=None, lat_col: str='LATITUDE', lon_col: str='LONGITUDE', presencia_col: str | None='Presencia', etiqueta_presencia_positiva: str='Positiva', dentro: bool=True, merged_original: pd.DataFrame | None=None, titulo: str='Checklists vs. Ecorregión', figsize=(9, 9), alpha_pts: float=0.8, color_presencia: str='royalblue', color_ausencia: str='tomato', color_contexto: str='gray', savepath: str | None=None):
    """
    Visualiza resultados de filtrar_checklists_por_region_fast con colores distintos
    para presencias (azul) y ausencias (rojo).
    Si se pasa merged_original, muestra también los puntos no incluidos (gris).
    """
    eco_union, eco_gdf = _load_region_union(region_path=region_path, region_gdf=region_gdf, eco_field=eco_field, eco_values=eco_values, buffer_km=buffer_km, simplify_tolerance_m=simplify_tolerance_m)
    df_kept = df_filtrado.dropna(subset=[lat_col, lon_col]).copy()
    gdf_kept = gpd.GeoDataFrame(df_kept, geometry=gpd.points_from_xy(df_kept[lon_col], df_kept[lat_col]), crs=4326)
    gdf_other = None
    if merged_original is not None:
        base = merged_original.dropna(subset=[lat_col, lon_col]).copy()
        gdf_all = gpd.GeoDataFrame(base, geometry=gpd.points_from_xy(base[lon_col], base[lat_col]), crs=4326)
        minx, miny, maxx, maxy = eco_union.bounds
        lon = gdf_all[lon_col].to_numpy()
        lat = gdf_all[lat_col].to_numpy()
        mask_bbox = (lon >= minx) & (lon <= maxx) & (lat >= miny) & (lat <= maxy)
        mask_preciso = np.zeros(len(gdf_all), dtype=bool)
        if mask_bbox.any():
            prep_poly = prep(eco_union)
            mask_preciso_bbox = np.fromiter((prep_poly.covers(p) for p in gdf_all.geometry.values[mask_bbox]), count=mask_bbox.sum(), dtype=bool)
            mask_preciso[mask_bbox] = mask_preciso_bbox
        mask_region_all = mask_preciso if dentro else ~mask_preciso
        other = gdf_all.loc[~mask_region_all] if dentro else gdf_all.loc[mask_region_all]
        keycols = [lat_col, lon_col]
        if len(gdf_kept) > 0:
            other = other.merge(gdf_kept[keycols], on=keycols, how='left', indicator=True)
            other = other[other['_merge'] == 'left_only'].drop(columns=['_merge'])
        gdf_other = other
    fig, ax = plt.subplots(figsize=figsize)
    eco_gdf.boundary.plot(ax=ax, linewidth=1.2, color='black')
    try:
        eco_gdf.plot(ax=ax, alpha=0.08, color='lightgray')
    except Exception:
        pass
    if gdf_other is not None and len(gdf_other) > 0:
        label_other = 'Fuera' if dentro else 'Dentro'
        gdf_other.plot(ax=ax, markersize=6, alpha=0.2, color=color_contexto, label=f'{label_other} (no kept)')
    if presencia_col and presencia_col in gdf_kept.columns:
        pos_mask = gdf_kept[presencia_col].astype(str) == str(etiqueta_presencia_positiva)
        gdf_pos = gdf_kept.loc[pos_mask]
        gdf_neg = gdf_kept.loc[~pos_mask]
        if len(gdf_neg) > 0:
            gdf_neg.plot(ax=ax, markersize=10, color=color_ausencia, alpha=alpha_pts, label='Ausencias')
        if len(gdf_pos) > 0:
            gdf_pos.plot(ax=ax, markersize=10, color=color_presencia, alpha=alpha_pts, label='Presencias')
    else:
        gdf_kept.plot(ax=ax, markersize=10, alpha=alpha_pts, color=color_presencia, label='Checklists')
    ax.set_xlabel('Longitud')
    ax.set_ylabel('Latitud')
    ax.set_title(titulo)
    ax.legend(loc='best')
    ax.grid(True, linestyle='--', linewidth=0.4, alpha=0.5)
    try:
        ax.set_aspect('equal', adjustable='datalim')
    except Exception:
        pass
    if savepath:
        plt.savefig(savepath, dpi=300, bbox_inches='tight')
    plt.show()

def _ensure_df(obj: _PathOrDF, encoding: str='auto', sep: str=',') -> pd.DataFrame:
    """Ensures the input is a DataFrame, reading from path/buffer if necessary."""
    if isinstance(obj, pd.DataFrame):
        return obj
    if isinstance(obj, (str, bytes, IO)):
        return _read_csv_auto(obj, encoding=encoding, sep=sep)
    raise TypeError(f'Entrada no soportada: {type(obj)}. Use una ruta a CSV, buffer o un DataFrame.')

def _read_csv_auto(path_or_buf: _PathOrBuf, encoding: str='auto', sep: str=',', **kwargs) -> pd.DataFrame:
    """Reads a CSV file attempting multiple encodings."""
    if encoding != 'auto':
        return pd.read_csv(path_or_buf, encoding=encoding, encoding_errors='replace', sep=sep, **kwargs)
    tried = []
    encodings_to_try = ['utf-8', 'utf-8-sig', 'latin1', 'cp1252']
    for enc in encodings_to_try:
        try:
            return pd.read_csv(path_or_buf, encoding=enc, encoding_errors='replace', sep=sep, **kwargs)
        except Exception as e:
            tried.append((enc, e))
            continue
    msg = '\n'.join([f'- {enc}: {err}' for enc, err in tried])
    raise UnicodeDecodeError('geobio', b'', 0, 0, f'No se pudo decodificar el CSV. Intentos:\n{msg}') from e

def crear_mapa_interactivo(datos: _PathOrDF, archivo_html='mapa_avistajes_interactivo.html', *, encoding: str='auto', sep: str=',') -> str:
    """
    Crea un mapa interactivo con los puntos de avistajes usando Folium.
    Acepta ruta a archivo CSV/TSV o DataFrame.
    """
    df = _ensure_df(datos, encoding=encoding, sep=sep).dropna(subset=['LATITUDE', 'LONGITUDE'])
    geometry = [Point(xy) for xy in zip(df.LONGITUDE, df.LATITUDE)]
    gdf = gpd.GeoDataFrame(df, geometry=geometry, crs='EPSG:4326')
    if gdf.empty:
        print('DataFrame vacío después de eliminar NaNs en lat/lon. No se puede crear el mapa.')
        return None
    centro_mapa = [gdf.geometry.y.mean(), gdf.geometry.x.mean()]
    m = folium.Map(location=centro_mapa, zoom_start=2)
    species_col = 'species' if 'species' in gdf.columns else 'SCIENTIFIC NAME'
    if species_col not in gdf.columns:
        species_col = 'COMMON NAME'
    if species_col not in gdf.columns:
        species_col = None
    for _, fila in gdf.iterrows():
        popup_text = 'Detalles no disponibles'
        if species_col:
            popup_text = f"Especie: {fila.get(species_col, 'N/A')}"
        if 'OBSERVATION DATE' in fila:
            popup_text += f"<br>Fecha: {fila['OBSERVATION DATE']}"
        if 'OBSERVER ID' in fila:
            popup_text += f"<br>Observador: {fila['OBSERVER ID']}"
        folium.CircleMarker(location=[fila.geometry.y, fila.geometry.x], radius=3, color='red', fill=True, fillOpacity=0.6, popup=folium.Popup(popup_text, max_width=300)).add_to(m)
    Draw(export=True, draw_options={'polygon': True, 'marker': False, 'circle': False, 'polyline': False, 'rectangle': True}, edit_options={'edit': True}).add_to(m)
    ruta_completa = os.path.abspath(archivo_html)
    try:
        m.save(archivo_html)
        print(f'Mapa interactivo guardado en: {ruta_completa}')
    except Exception as e:
        print(f'Error al guardar el mapa interactivo: {e}')
        ruta_completa = None
    return ruta_completa

def crear_visualizaciones_matplotlib(gdf_filtrado: gpd.GeoDataFrame, segmentos_info: dict):
    if gdf_filtrado.empty:
        print('DataFrame filtrado vacío. No se pueden crear visualizaciones de Matplotlib.')
        return
    if 'Segmento' not in gdf_filtrado.columns or gdf_filtrado['Segmento'].nunique() <= 1:
        print("Columna 'Segmento' no válida o con un solo valor después del filtrado. Saltando visualizaciones por segmento.")
        return
    plt.figure(figsize=(10, 6))
    gdf_filtrado['Segmento'] = gdf_filtrado['Segmento'].astype(str)
    gdf_filtrado['Segmento'].value_counts().plot(kind='bar', color='skyblue')
    plt.title('Número de observaciones por segmento')
    plt.xlabel('Segmento')
    plt.ylabel('Número de observaciones')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()
    plt.figure(figsize=(12, 8))
    colores = ['red', 'blue', 'green', 'orange', 'purple', 'brown', 'pink']
    valid_segments_plotted = 0
    for i, (nombre_segmento, info) in enumerate(segmentos_info.items()):
        segmento_gdf = info.get('gdf')
        if segmento_gdf is not None and (not segmento_gdf.empty):
            color = colores[i % len(colores)]
            plt.scatter(segmento_gdf.geometry.x, segmento_gdf.geometry.y, c=color, label=nombre_segmento, alpha=0.7, s=10)
            valid_segments_plotted += 1
    if valid_segments_plotted > 0:
        plt.xlabel('Longitud')
        plt.ylabel('Latitud')
        plt.title('Distribución de observaciones por segmento')
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.show()
    else:
        print('No hay datos válidos en los segmentos para mostrar distribución espacial.')

def crear_mapas_interactivos(gdf_filtrado: gpd.GeoDataFrame, segmentos_info: dict, centro_mapa, columna_especie: str):
    if gdf_filtrado.empty:
        print('DataFrame filtrado vacío. No se pueden crear mapas interactivos.')
        return
    m_todos = folium.Map(location=centro_mapa, zoom_start=2)
    colores = ['red', 'blue', 'green', 'orange', 'purple', 'darkred', 'lightred']
    valid_segments_mapped = 0
    for i, (nombre_segmento, info) in enumerate(segmentos_info.items()):
        poligono = info.get('poligono')
        segmento_gdf = info.get('gdf')
        if poligono is not None and segmento_gdf is not None and (not segmento_gdf.empty):
            color = colores[i % len(colores)]
            folium.GeoJson(poligono, style_function=lambda x, color=color: {'fillColor': color, 'color': color, 'weight': 2, 'fillOpacity': 0.1}, tooltip=nombre_segmento).add_to(m_todos)
            for _, fila in segmento_gdf.iterrows():
                popup_text = f"Especie: {fila.get(columna_especie, 'N/A')} - {nombre_segmento}"
                if 'OBSERVATION DATE' in fila:
                    popup_text += f"<br>Fecha: {fila['OBSERVATION DATE']}"
                folium.CircleMarker(location=[fila.geometry.y, fila.geometry.x], radius=3, color=color, fill=True, fillOpacity=0.7, popup=folium.Popup(popup_text, max_width=300)).add_to(m_todos)
            valid_segments_mapped += 1
    if valid_segments_mapped > 0:
        m_todos.save('mapa_segmentos_interactivo.html')
        print(f"Mapa interactivo de segmentos guardado en: {os.path.abspath('mapa_segmentos_interactivo.html')}")
    else:
        print('No hay datos válidos en los segmentos para mostrar en el mapa interactivo de segmentos.')
    if not gdf_filtrado.empty:
        df_for_plotly = gdf_filtrado.drop(columns=['geometry'], errors='ignore')
        fig = px.scatter_geo(df_for_plotly, lat='LATITUDE', lon='LONGITUDE', color='Segmento', hover_name=columna_especie, title='Observaciones segmentadas por polígonos', projection='natural earth')
        fig.write_html('mapa_segmentos_plotly.html')
        print(f"Mapa interactivo de segmentos Plotly guardado en: {os.path.abspath('mapa_segmentos_plotly.html')}")

def filtrar_por_poligonos(datos: _PathOrDF, ruta_poligono: str='data.geojson', columna_especie: str='ALL SPECIE', archivo_salida: str='datos_segmentados.csv', *, encoding: str='auto', sep: str=',') -> gpd.GeoDataFrame | None:
    """
    Filtra puntos de avistajes que caen dentro de polígonos definidos en un GeoJSON.
    Acepta ruta a archivo CSV/TSV o DataFrame.
    """
    df = _ensure_df(datos, encoding=encoding, sep=sep).dropna(subset=['LATITUDE', 'LONGITUDE'])
    if df.empty:
        print('DataFrame vacío después de eliminar NaNs en lat/lon. No se puede filtrar por polígonos.')
        return None
    geometry = [Point(xy) for xy in zip(df.LONGITUDE, df.LATITUDE)]
    gdf = gpd.GeoDataFrame(df, geometry=geometry, crs='EPSG:4326')
    centro_mapa = [gdf.geometry.y.mean(), gdf.geometry.x.mean()]
    try:
        poligonos_dibujados = gpd.read_file(ruta_poligono)
        if len(poligonos_dibujados) == 0:
            print('Error: El archivo GeoJSON no contiene polígonos')
            return None
        print(f'Se encontraron {len(poligonos_dibujados)} polígono(s) en el archivo: {ruta_poligono}')
        poligonos_dibujados = poligonos_dibujados.to_crs(gdf.crs)
        gdf_segmentado = gdf.copy()
        gdf_segmentado['Segmento'] = 'Fuera_de_poligonos'
        segmentos_info = {}
        for i, (_, poligono) in enumerate(poligonos_dibujados.iterrows()):
            nombre_segmento = f'Segmento_{i + 1}'
            geometria_poligono = poligono.geometry
            puntos_en_poligono_mask = gdf_segmentado.geometry.within(geometria_poligono)
            gdf_segmentado.loc[puntos_en_poligono_mask & (gdf_segmentado['Segmento'] == 'Fuera_de_poligonos'), 'Segmento'] = nombre_segmento
        gdf_filtrado = gdf_segmentado[gdf_segmentado['Segmento'] != 'Fuera_de_poligonos'].copy()
        for i, (_, poligono) in enumerate(poligonos_dibujados.iterrows()):
            nombre_segmento = f'Segmento_{i + 1}'
            gdf_segmento = gdf_filtrado[gdf_filtrado['Segmento'] == nombre_segmento]
            if not gdf_segmento.empty:
                segmentos_info[nombre_segmento] = {'gdf': gdf_segmento, 'poligono': poligono.geometry, 'estadisticas': {'total_puntos': len(gdf_segmento), 'especies_unicas': gdf_segmento[columna_especie].nunique() if columna_especie in gdf_segmento.columns else 'N/A'}}
                uniq = segmentos_info[nombre_segmento]['estadisticas']['especies_unicas']
                print(f'{nombre_segmento}: {len(gdf_segmento)} puntos, {uniq} especies únicas')
            else:
                print(f'{nombre_segmento}: 0 puntos')
        if not gdf_filtrado.empty:
            ruta_csv_salida = os.path.abspath(archivo_salida)
            df_filtrado_csv = gdf_filtrado.drop(columns=['geometry'], errors='ignore')
            df_filtrado_csv.to_csv(archivo_salida, index=False, encoding='utf-8')
            print(f'Datos segmentados guardados en: {ruta_csv_salida}')
        else:
            print('No se encontraron puntos dentro de los polígonos.')
        if not gdf_filtrado.empty and segmentos_info:
            crear_visualizaciones_matplotlib(gdf_filtrado, segmentos_info)
            if 'LATITUDE' in gdf.columns and 'LONGITUDE' in gdf.columns and (columna_especie in gdf.columns):
                fig_original = px.scatter_geo(gdf.drop(columns=['geometry'], errors='ignore'), lat='LATITUDE', lon='LONGITUDE', hover_name=columna_especie, title='Avistamientos (Todos)', projection='natural earth')
                fig_original.show()
            else:
                print(f"Faltan columnas ('LATITUDE', 'LONGITUDE', o '{columna_especie}') para el mapa Plotly original.")
            if 'LATITUDE' in gdf_filtrado.columns and 'LONGITUDE' in gdf_filtrado.columns and ('Segmento' in gdf_filtrado.columns) and (columna_especie in gdf_filtrado.columns):
                fig_filtrado = px.scatter_geo(gdf_filtrado.drop(columns=['geometry'], errors='ignore'), lat='LATITUDE', lon='LONGITUDE', color='Segmento', hover_name=columna_especie, title='Avistamientos Filtrados por Polígonos', projection='natural earth')
                fig_filtrado.show()
            else:
                print(f"Faltan columnas ('LATITUDE', 'LONGITUDE', 'Segmento', o '{columna_especie}') para el mapa Plotly filtrado.")
            crear_mapas_interactivos(gdf_filtrado, segmentos_info, centro_mapa, columna_especie)
        print('\n--- RESUMEN ESTADÍSTICO ---')
        print(f'Total de puntos originales: {len(gdf)}')
        if len(gdf) > 0:
            print(f'Total de puntos dentro de polígonos: {len(gdf_filtrado)} ({len(gdf_filtrado) / len(gdf) * 100:.1f}%)')
        for nombre_segmento, info in segmentos_info.items():
            total_puntos_segmento = info.get('estadisticas', {}).get('total_puntos', 0)
            print(f'{nombre_segmento}: {total_puntos_segmento} puntos ({(total_puntos_segmento / len(gdf) * 100 if len(gdf) > 0 else 0):.1f}%)')
        return gdf_filtrado
    except FileNotFoundError:
        print(f'Error: No se encontró el archivo {ruta_poligono}')
        print('Asegúrate de haber dibujado y guardado los polígonos primero.')
        return None
    except Exception as e:
        print(f'Error al procesar los polígonos: {e}')
        import traceback
        traceback.print_exc()
        return None

def cargar_datos_segmentados(datos: _PathOrDF, *, encoding: str='auto', sep: str=',') -> pd.DataFrame:
    """
    Carga datos segmentados desde un archivo CSV o un DataFrame.
    """
    df = _ensure_df(datos, encoding=encoding, sep=sep)
    print(f'Total de observaciones segmentadas: {len(df)}')
    if 'Segmento' in df.columns:
        print(f"Segmentos encontrados: {df['Segmento'].unique().tolist()}")
        print('Número de observaciones por segmento:')
        print(df['Segmento'].value_counts())
    return df

def add_season(datos) -> pd.DataFrame:
    """
    Agrega una columna 'Estación' según el mes.
    Acepta tanto una ruta CSV como un DataFrame.
    """
    if isinstance(datos, str):
        df = pd.read_csv(datos, encoding='latin1')
    elif isinstance(datos, pd.DataFrame):
        df = datos.copy()
    else:
        raise TypeError("El parámetro 'datos' debe ser una ruta CSV (str) o un DataFrame.")
    season_map = {12: 'Verano', 1: 'Verano', 2: 'Verano', 3: 'Otoño', 4: 'Otoño', 5: 'Otoño', 6: 'Invierno', 7: 'Invierno', 8: 'Invierno', 9: 'Primavera', 10: 'Primavera', 11: 'Primavera'}
    if 'MONTH' not in df.columns:
        raise KeyError("La columna 'MONTH' no existe en el DataFrame. Ejecute primero separar_fechas().")
    df['Estación'] = df['MONTH'].map(season_map)
    print("Columna 'Estación' agregada:")
    display(df[['MONTH', 'Estación']].head())
    return df

def filter_seasons(datos, keep=('Verano', 'Invierno'), modify=False) -> pd.DataFrame:
    """
    Filtra las estaciones (por defecto solo Verano e Invierno).
    Acepta tanto una ruta CSV como un DataFrame.
    """
    if isinstance(datos, str):
        df = pd.read_csv(datos, encoding='latin1')
    elif isinstance(datos, pd.DataFrame):
        df = datos.copy()
    else:
        raise TypeError("El parámetro 'datos' debe ser una ruta CSV (str) o un DataFrame.")
    if 'Estación' not in df.columns:
        raise KeyError("La columna 'Estación' no existe. Ejecute primero add_season().")
    if modify:
        df.drop(df[~df['Estación'].isin(keep)].index, inplace=True)
        print(f'Filtrado in-place. Estaciones mantenidas: {keep}')
        return df
    else:
        df_filtrado = df[df['Estación'].isin(keep)].copy()
        print(f'Se generó nuevo DataFrame filtrado por estaciones: {keep}')
        return df_filtrado

def split_segments(datos) -> dict:
    """
    Divide el DataFrame en segmentos S1-S5 según la columna 'Segmento'.
    Acepta tanto una ruta CSV como un DataFrame.
    Retorna un diccionario con claves 'S1', 'S2', 'S3', 'S4', 'S5'.
    """
    if isinstance(datos, str):
        df = pd.read_csv(datos, encoding='latin1')
    elif isinstance(datos, pd.DataFrame):
        df = datos.copy()
    else:
        raise TypeError("El parámetro 'datos' debe ser una ruta CSV (str) o un DataFrame.")
    if 'Segmento' not in df.columns:
        raise KeyError("La columna 'Segmento' no existe en el DataFrame.")
    segmentos = {f'S{i}': df[df['Segmento'] == i].copy() for i in range(1, 6)}
    for nombre, seg_df in segmentos.items():
        print(f'{nombre}: {len(seg_df)} registros')
    return segmentos

def extraer_variables_especificas(csv_avistamientos, diccionario_rasters, archivo_salida='datos_con_variables.csv'):
    """
    Extrae variables ambientales específicas con nombres personalizados.
    Aplica la conversión adecuada para las variables de temperatura de CHELSA v2.1.

    Temperatura: valor*0.1 - 273.15 (°C)
    Precipitación: sin conversión (mm)
    """
    import numpy as np
    import matplotlib.pyplot as plt
    import pandas as pd
    import seaborn as sns
    from shapely.geometry import Point
    import geopandas as gpd
    import os
    import rasterio
    from rasterio.mask import mask
    from shapely.geometry import Point
    if isinstance(csv_avistamientos, pd.DataFrame):
        df = csv_avistamientos.copy()
    else:
        df = pd.read_csv(csv_avistamientos)
    geometry = [Point(xy) for xy in zip(df.LONGITUDE, df.LATITUDE)]
    gdf = gpd.GeoDataFrame(df, geometry=geometry, crs='EPSG:4326')
    for nombre_variable, ruta_raster in diccionario_rasters.items():
        print(f'Procesando: {nombre_variable}')
        if not os.path.exists(ruta_raster):
            print(f'  Error: No se encuentra el archivo {ruta_raster}')
            gdf[nombre_variable] = None
            continue
        try:
            with rasterio.open(ruta_raster) as src:
                coordenadas = [(punto.x, punto.y) for punto in gdf.geometry]
                valores = []
                for valor in src.sample(coordenadas):
                    v = valor[0]
                    if np.isnan(v):
                        valores.append(None)
                    else:
                        if any((bio in os.path.basename(ruta_raster).lower() for bio in [f'bio{i}' for i in range(1, 12)])):
                            v = v * 0.1 - 273.15
                        valores.append(v)
                gdf[nombre_variable] = valores
                valores_validos = [v for v in valores if v is not None]
                if valores_validos:
                    print(f'  Extraídos: {len(valores_validos)}/{len(valores)} valores')
                    print(f'  Rango: {min(valores_validos):.2f} – {max(valores_validos):.2f}')
                    print(f'  Media: {np.mean(valores_validos):.2f}')
                else:
                    print('  No se pudieron extraer valores')
        except Exception as e:
            print(f'  Error: {e}')
            gdf[nombre_variable] = None
    df_salida = gdf.drop(columns=['geometry']) if 'geometry' in gdf.columns else gdf.copy()
    df_salida.to_csv(archivo_salida, index=False)
    print(f'\nDatos guardados en: {os.path.abspath(archivo_salida)}')
    return gdf

def separar_fechas(ruta_archivo):
    """
  Separa la columna 'OBSERVATION DATE' en 'DAY', 'MONTH', y 'YEAR'.
  Puede recibir una ruta de archivo (str) o un DataFrame.
  """
    if isinstance(ruta_archivo, pd.DataFrame):
        df = ruta_archivo.copy()
    else:
        df = pd.read_csv(ruta_archivo, encoding='latin1')
    df['OBSERVATION DATE'] = pd.to_datetime(df['OBSERVATION DATE'], format='ISO8601', errors='coerce')
    valid_dates = df['OBSERVATION DATE'].dropna()
    df['DAY'] = np.nan
    df['MONTH'] = np.nan
    df['YEAR'] = np.nan
    df.loc[valid_dates.index, 'DAY'] = valid_dates.dt.day
    df.loc[valid_dates.index, 'MONTH'] = valid_dates.dt.month
    df.loc[valid_dates.index, 'YEAR'] = valid_dates.dt.year
    for col in ['DAY', 'MONTH', 'YEAR']:
        if not df[col].dropna().empty and df[col].dropna().apply(float.is_integer).all():
            df[col] = df[col].astype('Int64')
    print('Columnas de fecha separadas:')
    display(df[['OBSERVATION DATE', 'DAY', 'MONTH', 'YEAR']].head())
    return df

def plot_latitude_density(df: pd.DataFrame):
    """Frecuencia de observaciones / Latitud."""
    sns.kdeplot(data=df, x='LATITUDE', hue='Estación', fill=True, alpha=0.5, palette={'Verano': 'green', 'Invierno': 'blue', 'Otoño': 'orange', 'Primavera': 'pink'})
    plt.xlabel('Latitud')
    plt.ylabel('Frecuencia de observaciones')
    plt.show()

def plot_temperature_density(df: pd.DataFrame):
    """Frecuencia de observaciones / Temperatura."""
    xlim = (df['Temperatura'].min(), df['Temperatura'].max())
    sns.kdeplot(data=df, x='Temperatura', hue='Estación', fill=True, alpha=0.5, palette={'Verano': 'green', 'Invierno': 'blue', 'Otoño': 'orange', 'Primavera': 'pink'})
    plt.xlabel('Temperatura media anual')
    plt.ylabel('Frecuencia de observaciones')
    plt.xlim(xlim)
    plt.show()

def plot_precipitation_density(df: pd.DataFrame):
    """Frecuencia de observaciones / Precipitación."""
    xlim = (df['Precipitacion'].min(), df['Precipitacion'].max())
    sns.kdeplot(data=df, x='Precipitacion', hue='Estación', fill=True, alpha=0.5, palette={'Verano': 'green', 'Invierno': 'blue', 'Otoño': 'orange', 'Primavera': 'pink'})
    plt.xlabel('Precipitación total anual')
    plt.ylabel('Frecuencia de observaciones')
    plt.xlim(xlim)
    plt.show()

def plot_precipitation_by_segment(df: pd.DataFrame):
    """Precipitación por segmento."""
    xlim = (df['Precipitacion'].min(), df['Precipitacion'].max())
    for seg in df['Segmento'].unique():
        subset = df[df['Segmento'] == seg]
        sns.kdeplot(data=subset, x='Precipitacion', hue='Estación', fill=True, alpha=0.5, palette={'Verano': 'green', 'Invierno': 'blue', 'Otoño': 'orange', 'Primavera': 'pink'})
        plt.title(f'Segmento {seg}')
        plt.xlabel('Precipitación total anual')
        plt.ylabel('Frecuencia de observaciones')
        plt.xlim(xlim)
        plt.show()
        print(f"Media precipitación Segmento {seg}: {subset['Precipitacion'].mean():.2f}")

def plot_altitude_density(df: pd.DataFrame):
    """Frecuencia de observaciones / Altitud."""
    xlim = (df['Altitud'].min(), df['Altitud'].max())
    sns.kdeplot(data=df, x='Altitud', hue='Estación', fill=True, alpha=0.5, palette={'Verano': 'green', 'Invierno': 'blue', 'Otoño': 'orange', 'Primavera': 'pink'})
    plt.xlabel('Altitud')
    plt.ylabel('Frecuencia de observaciones')
    plt.xlim(xlim)
    plt.show()

def plot_latitude_by_segment(df: pd.DataFrame):
    """Gráfico de LATITUDE por Estación, separado por Segmento."""
    for seg in df['Segmento'].unique():
        subset = df[df['Segmento'] == str(seg)].copy()
        subset['Estación'] = pd.Categorical(subset['Estación'], categories=['Verano', 'Otoño', 'Invierno', 'Primavera'], ordered=True)
        sns.boxplot(data=subset, x='Estación', y='LATITUDE', hue='Estación', palette=colores_est, legend=False)
        sns.pointplot(data=subset, x='Estación', y='LATITUDE', estimator=np.mean, color='red', markers='D', linestyle='none')
        plt.title(f'Segmento {seg}')
        plt.xlabel('Estación')
        plt.ylabel('Latitud')
        plt.show()

def plot_altitude_by_segment(df: pd.DataFrame):
    """Altitud / Estaciones por segmento."""
    for seg in df['Segmento'].unique():
        subset = df[df['Segmento'] == str(seg)].copy()
        subset['Estación'] = pd.Categorical(subset['Estación'], categories=['Verano', 'Otoño', 'Invierno', 'Primavera'], ordered=True)
        sns.boxplot(data=subset, x='Estación', y='Altitud', palette=colores_est)
        sns.pointplot(data=subset, x='Estación', y='Altitud', estimator=np.mean, color='red', join=False, markers='D')
        plt.title(f'Segmento {seg}')
        plt.show()

def plot_monthly_frequencies(df: pd.DataFrame):
    """Frecuencia proporcional mensual por segmento."""
    freq = df.groupby(['MONTH', 'Segmento']).size().reset_index(name='Conteo')
    freq['Frecuencia'] = freq.groupby('MONTH')['Conteo'].transform(lambda x: x / x.sum())
    segmentos_unicos = sorted(df['Segmento'].unique())
    colores = sns.color_palette('Spectral', n_colors=len(segmentos_unicos))
    plt.figure(figsize=(10, 6))
    sns.barplot(data=freq, x='MONTH', y='Frecuencia', hue='Segmento', palette=colores, edgecolor='black')
    plt.title('Frecuencia proporcional mensual por segmento')
    plt.xlabel('Mes')
    plt.ylabel('Frecuencia proporcional')
    plt.legend(title='Segmento')
    plt.ylim(0, 1)
    plt.tight_layout()
    plt.show()

def plot_altitude_all_segments(df: pd.DataFrame):
    """Altitud / Segmentos todos juntos con curva LOESS."""
    Altitud = pd.DataFrame({'Poblacion': [1, 2, 3, 4, 5], 'Max': [5218, 5833, 6273, 5693, 2652], 'Min': [0, 0, 0, 0, 0]})
    xseq = np.linspace(0.2, 5.8, 200)
    loess_fit = lowess(Altitud['Max'], Altitud['Poblacion'], frac=0.8, it=0)
    pred = np.interp(xseq, loess_fit[:, 0], loess_fit[:, 1])
    Curva = pd.DataFrame({'Poblacion': xseq, 'Max': pred})
    sns.boxplot(data=df, x='Segmento', y='Altitud', hue='Estación', palette=colores_est)
    sns.pointplot(data=df, x='Segmento', y='Altitud', estimator=np.mean, color='red', linestyle='none', markers='D')
    plt.plot(Curva['Poblacion'], Curva['Max'], color='grey', linewidth=1)
    plt.xlabel('Segmento')
    plt.ylabel('Altitud')
    plt.legend(bbox_to_anchor=(1, -0.1), loc='upper center', ncol=2)
    plt.show()

def plot_mirror_presence_by_station(df: pd.DataFrame, coord_col: str='LATITUDE', presence_col: str='Presencia', positive_value: str='Positiva', station_col: str='Estacion', stations=None, station_order=None, bin_size: float=1.0, title: str | None=None, xlabel: str | None=None, figsize=(12, 8), bar_alpha: float=1.0, grid_alpha: float=0.3, show_share_labels: bool=True, share_decimals: int=0, share_fmt: str='{:.0f}%', min_height_for_label_bot: float=3, show_share_labels_top: bool=True, min_height_for_label_top: float=3, min_share_for_label: float=5.0, label_nudge: float=0.04, outside_when_small: bool=True, outside_height_thresh: float=2.0, outside_offset_frac: float=0.06, arrow_linewidth: float=0.8, arrow_shrinkA: float=0.0, arrow_shrinkB: float=0.0):
    """
    Barras espejadas por bins de `coord_col`:
    - Arriba: conteo de positivos, apilado por estación.
    - Abajo: conteo total, apilado por estación (eje Y invertido).

    Etiquetas:
    - % por estación dentro del bin en ambos paneles.
    - Anti-solapamiento: umbral de altura mínima, % mínimo, nudge alternado.
    - Etiquetas externas con flecha cuando el segmento es muy chico.
    """
    if coord_col not in df.columns:
        raise ValueError(f'`{coord_col}` no está en el DataFrame.')
    if presence_col not in df.columns:
        raise ValueError(f'`{presence_col}` no está en el DataFrame.')
    work = df.copy()
    if station_col not in work.columns:
        station_col = '_station_dummy_'
        work[station_col] = 'Todas'
    if stations is not None:
        work = work[work[station_col].isin(stations)].copy()
    present_stations = pd.Index(work[station_col].dropna().unique())
    if stations is None:
        stations_used = present_stations.tolist()
    else:
        stations_used = [s for s in stations if s in present_stations]
    if not stations_used:
        raise ValueError('No hay filas luego de filtrar por las estaciones solicitadas.')
    if station_order is None:
        station_order = stations_used
    else:
        station_order = [s for s in station_order if s in stations_used]
    work = work.dropna(subset=[coord_col])
    cmin = np.floor(work[coord_col].min() / bin_size) * bin_size
    cmax = np.ceil(work[coord_col].max() / bin_size) * bin_size
    bins = np.arange(cmin, cmax + bin_size, bin_size)
    if len(bins) < 2:
        bins = np.array([work[coord_col].min(), work[coord_col].max() + bin_size])
    work['BIN'] = pd.cut(work[coord_col], bins=bins, include_lowest=True)
    tot = work.groupby(['BIN', station_col]).size().unstack(station_col).reindex(columns=station_order).fillna(0).astype(int)
    pos = work[work[presence_col] == positive_value].groupby(['BIN', station_col]).size().unstack(station_col).reindex(columns=station_order).reindex(index=tot.index, fill_value=0).fillna(0).astype(int)
    centers = np.array([iv.mid for iv in tot.index])
    width = bin_size * 0.9 if bin_size > 0 else 0.9
    fig = plt.figure(figsize=figsize)
    gs = gridspec.GridSpec(2, 1, height_ratios=[1, 1], hspace=0.05)
    ax_top = fig.add_subplot(gs[0])
    ax_bot = fig.add_subplot(gs[1], sharex=ax_top)
    bottom_top = np.zeros(len(centers))
    for s in station_order:
        vals = pos[s].values if s in pos.columns else np.zeros(len(centers))
        ax_top.bar(centers, vals, width=width, bottom=bottom_top, alpha=bar_alpha, label=str(s))
        bottom_top += vals
    ax_top.set_ylabel('Positivos')
    ax_top.grid(alpha=grid_alpha)
    ax_top.legend(loc='upper right', title='Estación')
    bottom_bot = np.zeros(len(centers))
    for s in station_order:
        vals = tot[s].values if s in tot.columns else np.zeros(len(centers))
        ax_bot.bar(centers, vals, width=width, bottom=bottom_bot, alpha=bar_alpha, label=str(s))
        bottom_bot += vals
    ax_bot.invert_yaxis()
    ax_bot.set_ylabel('Total')
    ax_bot.set_xlabel(xlabel if xlabel is not None else coord_col)
    ax_bot.grid(alpha=grid_alpha)

    def _label_shares(ax, table_counts, is_inverted_axis: bool, min_height_for_label: float):
        """
        Coloca % dentro del bin por estación.
        - Si el segmento es >= min_height_for_label y el % >= min_share_for_label -> etiqueta interna con nudge alternado.
        - Si outside_when_small y h < outside_height_thresh -> etiqueta externa con flecha apuntando al centro del segmento.
        """
        bin_sums = table_counts.sum(axis=1).replace(0, np.nan)
        shares = (table_counts.div(bin_sums, axis=0) * 100).fillna(0)
        y0, y1 = ax.get_ylim()
        yrange = abs(y1 - y0)
        for i, bin_idx in enumerate(table_counts.index):
            total_bin = table_counts.loc[bin_idx].sum()
            if total_bin == 0:
                continue
            y_prev = 0.0
            nflip = 1
            for s in station_order:
                h = float(table_counts.loc[bin_idx, s])
                if h <= 0:
                    continue
                share_val = float(shares.loc[bin_idx, s])
                y_center = y_prev + h / 2.0
                place_inside = h >= min_height_for_label and share_val >= min_share_for_label
                place_outside = outside_when_small and h < outside_height_thresh and (share_val >= min_share_for_label)
                if place_inside:
                    y_center_adj = y_center + nflip * (h * label_nudge)
                    nflip *= -1
                    ax.text(centers[i], y_center_adj, share_fmt.format(round(share_val, share_decimals)), ha='center', va='center', fontsize=9, clip_on=True)
                elif place_outside:
                    x0 = centers[i]
                    y_seg = y_center
                    sign = +1 if is_inverted_axis else +1
                    y_text = y_seg + sign * (outside_offset_frac * yrange)
                    ax.annotate(share_fmt.format(round(share_val, share_decimals)), xy=(x0, y_seg), xytext=(x0, y_text), ha='center', va='center', fontsize=9, arrowprops=dict(arrowstyle='-|>', lw=arrow_linewidth, shrinkA=arrow_shrinkA, shrinkB=arrow_shrinkB))
                y_prev += h
    if show_share_labels and len(tot) > 0:
        _label_shares(ax_bot, tot, is_inverted_axis=True, min_height_for_label=min_height_for_label_bot)
    if show_share_labels_top and len(pos) > 0:
        _label_shares(ax_top, pos, is_inverted_axis=False, min_height_for_label=min_height_for_label_top)
    if title is None:
        title = f'Distribución por {coord_col} — espejo por estación (% por bin en total y positivos; etiquetas externas si segmento chico)'
    fig.suptitle(title, y=0.98)
    for ax in (ax_top, ax_bot):
        ax.label_outer()
    plt.tight_layout()
    return (fig, (ax_top, ax_bot))
