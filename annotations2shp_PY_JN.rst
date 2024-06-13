.. code:: ipython3

    from matplotlib import pyplot as plt
    import json
    from pathlib import Path
    import geopandas as gpd
    import pandas as pd
    import contextily as cx
    import numpy as np
    
    
    class Annotations(object):
        def __init__(self, input_dir):
            self.input_dir = input_dir
            self.gdf = self.load_dir()
    
        def load_dir(self):
            files = Path(input_dir).glob('*.json')
            df = pd.DataFrame()
            for file in files:
                _gdf = self.load_annotation(file)
                df = pd.concat([df, _gdf], ignore_index=True)
            gdf = gpd.GeoDataFrame(df, geometry='geometry', crs='epsg:4326')
            return gdf
    
        def load_annotation(self, file):
            with open(file) as f:
                js = json.load(f)
            gdf = self.parse_annotation(js)
            gdf['fname'] = file.with_suffix('').name
            return gdf
    
        def parse_annotation(self, js):
            df = pd.DataFrame()
            for feature in js['features']:
                df_properties = pd.DataFrame.from_records([feature['properties']])
                df_features = pd.DataFrame(feature['features'])
                gdf_features = gpd.GeoDataFrame.from_features(feature['features'])
                _filter = gdf_features.geometry.geom_type == 'Polygon'
                gdf_features.loc[_filter, 'geometry'] = gdf_features[_filter].geometry.buffer(0)
                # expland style
                df_features = df_features.join(df_features['style'].apply(lambda x: pd.Series(x[0])), rsuffix='_s')
                # expand properties
                df_features = df_features.join(df_features['properties'].apply(pd.Series), rsuffix='_p')
                df_features.geometry = gdf_features.geometry
                df_join = df_features.merge(df_properties, how="cross", suffixes=['_f', ''])
                df = pd.concat([df, df_join], ignore_index=True)
    
            gdf = gpd.GeoDataFrame(df, geometry='geometry', crs="epsg:4326")
            gdf.drop(['properties', 'style', 'type', 'editing',
                      'highlight', 'id_f', 'canEdit',
                      'id_p', 'isValidFeature', 'id',
                      'dashArray'],
                     axis=1, inplace=True)
            # if circle annotations
            if 'polygonGeom' in gdf.columns:
                gdf.drop(['polygonGeom', 'center'], inplace=True, axis=1)
            # if icons
            if 'iconAnchor' in gdf.columns:
                gdf.drop(['iconAnchor'], inplace=True, axis=1)
            return gdf
    
        def plot(self, filter_expr=None, figsize=[10, 10], limits=None, ticks=True, grid=True):
            # plot using color values in dataframe
            gdf = self.gdf.to_crs(epsg=3857)
            # plot polygon boundaries
            _filter = gdf.geom_type.isin(['Polygon'])
            _gdf = gdf[_filter]
            if filter_expr:
                _gdf = _gdf.query(filter_expr)
            ax = _gdf.boundary.plot(color=_gdf.color, alpha=_gdf.opacity, figsize=figsize)
            # plot polygon fill
            ax = _gdf.plot(ax=ax,
                           color=_gdf.fillColor.fillna('None'),
                           alpha=_gdf.fillOpacity)
            # plot point and lines
            _filter = gdf.geom_type.isin(['Point', 'LineString'])
            _gdf = gdf[_filter]
            if filter_expr:
                _gdf = _gdf.query(filter_expr)
    
            _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),
                      alpha=_gdf.opacity.fillna(1))
    
            if limits is not None:
                ax.set_xlim([limits[0], limits[2]])
                ax.set_ylim([limits[1], limits[3]])
            if grid:
                ax.grid(True)
            if not ticks:
                ax.set_xticklabels([])
                ax.set_yticklabels([])
            # plot OSM
            cx.add_basemap(ax)
    
            return ax
    
        def save(self, output_dir, prefix):
            output_dir_path = Path(output_dir)
            gdf = self.gdf
            gtypes = ['Point', 'LineString', 'Polygon']
            for gtype in gtypes:
                _filter = gdf.geom_type == gtype
                _gdf = gpd.GeoDataFrame(gdf[_filter])
                file_path = output_dir_path / f"{prefix}_{gtype}.shp"
                _gdf.to_file(file_path)



.. parsed-literal::

    Matplotlib is building the font cache; this may take a moment.


.. code:: ipython3

    input_dir = '/home/mele/Notebooks/convert_annotations/input'
    output_dir = '/home/mele/Notebooks/convert_annotations/output'
    
    annotations = Annotations(input_dir)
    total_bounds = annotations.gdf.to_crs("3857").buffer(10000).total_bounds
    annotations.save(output_dir, 'annotation')
    annotations.gdf.to_csv(Path(output_dir) / 'gdf.csv')
    # plot by fname
    for fname in annotations.gdf.fname.unique():
        ax = annotations.plot(f"fname == '{fname}'", limits=total_bounds, ticks=False)
        ax.set_title(fname)
        plt.savefig(Path(output_dir) / 'thumbnails' / f'{fname}.png',
                    dpi=200,
                    bbox_inches='tight')
        plt.show()
    #%%
    # plot by title
    for fname in annotations.gdf.fname.unique():
        for title in annotations.gdf[annotations.gdf.fname==fname].title.unique():
            ax = annotations.plot(f"fname == '{fname}' and title == '{title}'", limits=total_bounds, ticks=False)
            ax.set_title(f"{fname} - {title}")
            plt.savefig(Path(output_dir) / 'thumbnails' / f'{fname}_{title}.png', dpi=200, bbox_inches='tight')
            plt.show()



.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:104: UserWarning: Column names longer than 10 characters will be truncated when saved to ESRI Shapefile.
      _gdf.to_file(file_path)
    /tmp/ipykernel_1613230/2108761174.py:104: UserWarning: Column names longer than 10 characters will be truncated when saved to ESRI Shapefile.
      _gdf.to_file(file_path)
    /tmp/ipykernel_1613230/2108761174.py:104: UserWarning: Column names longer than 10 characters will be truncated when saved to ESRI Shapefile.
      _gdf.to_file(file_path)



.. image:: output_1_1.png



.. image:: output_1_2.png



.. image:: output_1_3.png



.. image:: output_1_4.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:69: UserWarning: The GeoSeries you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.boundary.plot(color=_gdf.color, alpha=_gdf.opacity, figsize=figsize)
    /tmp/ipykernel_1613230/2108761174.py:71: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.plot(ax=ax,



.. image:: output_1_6.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_8.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_10.png



.. image:: output_1_11.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_13.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_15.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_17.png



.. image:: output_1_18.png



.. image:: output_1_19.png



.. image:: output_1_20.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:69: UserWarning: The GeoSeries you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.boundary.plot(color=_gdf.color, alpha=_gdf.opacity, figsize=figsize)
    /tmp/ipykernel_1613230/2108761174.py:71: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.plot(ax=ax,



.. image:: output_1_22.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_24.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_26.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_28.png



.. image:: output_1_29.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_31.png



.. image:: output_1_32.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:69: UserWarning: The GeoSeries you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.boundary.plot(color=_gdf.color, alpha=_gdf.opacity, figsize=figsize)
    /tmp/ipykernel_1613230/2108761174.py:71: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.plot(ax=ax,



.. image:: output_1_34.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:69: UserWarning: The GeoSeries you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.boundary.plot(color=_gdf.color, alpha=_gdf.opacity, figsize=figsize)
    /tmp/ipykernel_1613230/2108761174.py:71: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.plot(ax=ax,



.. image:: output_1_36.png



.. image:: output_1_37.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_39.png



.. image:: output_1_40.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_42.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_44.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_46.png



.. image:: output_1_47.png



.. image:: output_1_48.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_50.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_52.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_54.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_56.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:80: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      _gdf.plot(ax=ax, color=_gdf.color.fillna('None'),



.. image:: output_1_58.png



.. image:: output_1_59.png


.. parsed-literal::

    /tmp/ipykernel_1613230/2108761174.py:69: UserWarning: The GeoSeries you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.boundary.plot(color=_gdf.color, alpha=_gdf.opacity, figsize=figsize)
    /tmp/ipykernel_1613230/2108761174.py:71: UserWarning: The GeoDataFrame you are attempting to plot is empty. Nothing has been displayed.
      ax = _gdf.plot(ax=ax,



.. image:: output_1_61.png






.. parsed-literal::

    '/home/smenegon/notebooks'


