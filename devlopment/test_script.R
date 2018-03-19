#### TESTS #####
ga_save_settings(tracking_id="1",client_id=1)
ga_load_settings()
galog
ga_save_settings(tracking_id="2",client_id="2")
ga_load_settings()
ga_set_url()
galog$url
ga_delete_settings()
