#### TESTS #####
ga_save_settings(tracking_id="1",client_id=1)
ga_load_settings()
galog
ga_save_settings(tracking_id="2",client_id="2")
ga_load_settings()
ga_set_url()
galog$url
ga_delete_settings()

ga_initialize(tracking_id="UA-113884719-2",consent=TRUE,hostname="test.test.com")
user <- ga_set_user_id(user_id="Testy")
user <-ga_init_user(user_id="Testy")
ga_collect_pageview(page="/hello", user_id = user)
ga_collect_event(user=user,event_category = "Ping",event_action = "I touched you")
