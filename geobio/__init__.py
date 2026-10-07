from __future__ import annotations
"""GeoBio 2.0 — consolidated utilities

Bridging biology and data through code and creativity.
Contact: thbiodata@gmail.com
"""

from .core import (load_data, build_presence_from_checklists, filter_data, _pick_metric_epsg_from_pts, plot_checklists_radio_static, filtrar_checklists_por_region_fast, _pick_metric_epsg_from_geom, _load_region_union, plot_region_checklists_from_fast, _ensure_df, _read_csv_auto, crear_mapa_interactivo, crear_visualizaciones_matplotlib, crear_mapas_interactivos, filtrar_por_poligonos, cargar_datos_segmentados, add_season, filter_seasons, split_segments, extraer_variables_especificas, separar_fechas, plot_latitude_density, plot_temperature_density, plot_precipitation_density, plot_precipitation_by_segment, plot_altitude_density, plot_latitude_by_segment, plot_altitude_by_segment, plot_monthly_frequencies, plot_altitude_all_segments, plot_mirror_presence_by_station)

__all__ = ['load_data', 'build_presence_from_checklists', 'filter_data', '_pick_metric_epsg_from_pts', 'plot_checklists_radio_static', 'filtrar_checklists_por_region_fast', '_pick_metric_epsg_from_geom', '_load_region_union', 'plot_region_checklists_from_fast', '_ensure_df', '_read_csv_auto', 'crear_mapa_interactivo', 'crear_visualizaciones_matplotlib', 'crear_mapas_interactivos', 'filtrar_por_poligonos', 'cargar_datos_segmentados', 'add_season', 'filter_seasons', 'split_segments', 'extraer_variables_especificas', 'separar_fechas', 'plot_latitude_density', 'plot_temperature_density', 'plot_precipitation_density', 'plot_precipitation_by_segment', 'plot_altitude_density', 'plot_latitude_by_segment', 'plot_altitude_by_segment', 'plot_monthly_frequencies', 'plot_altitude_all_segments', 'plot_mirror_presence_by_station']
__version__ = "2.0.6"
__author__ = "Tiago Alejo Herrador | TH BioData Consulting"
