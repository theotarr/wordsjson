def standardize_lists(json):
    """
    Standardize the output of the API to convert keys with possible list values to always be lists.
    param json: The json object to standardize
    """

    # check edge case where a single UNKNOWN word is looked up
    if not isinstance(json["words"]["word"], list):
        json["words"]["word"] = [json["words"]["word"]]

    for word in json["words"]["word"]:
        if "unknown" in word:
            continue
        if not isinstance(word["entry"], list):
            word["entry"] = [word["entry"]]

        if not isinstance(word["entry"][0]["dict"], list):
            word["entry"][0]["dict"] = [word["entry"][0]["dict"]]

        if not isinstance(word["entry"][0]["mean"], list):
            word["entry"][0]["mean"] = [word["entry"][0]["mean"]]

        if not isinstance(word["entry"][0]["infl"], list):
            word["entry"][0]["infl"] = [word["entry"][0]["infl"]]

    return json

    



