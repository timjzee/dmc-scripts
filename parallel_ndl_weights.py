from pyndl import ndl

tens_path = "/vol/tensusers/timzee/cgn/"

weights = ndl.ndl(events=tens_path + 'ndl_ifadv.tab.gz', alpha=0.1, betas=(0.1, 0.1), method='openmp', remove_duplicates=False)

# weights.to_netcdf(tens_path + 'ifadv_ndl_weights.nc')
# xarray_extras.csv.to_csv(weights, tens_path + 'ifadv_pyndl_weights.csv')
weights.to_pandas().to_csv(tens_path + 'ifadv_pyndl_weights.csv')
