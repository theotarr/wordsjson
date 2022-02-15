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

        for entry in word["entry"]:
            if "dict" in entry and not isinstance(entry["dict"], list):
                entry["dict"] = [entry["dict"]]

            if "mean" in entry and not isinstance(entry["mean"], list):
                entry["mean"] = [entry["mean"]]

            if "infl" in entry and not isinstance(entry["infl"], list):
                entry["infl"] = [entry["infl"]]

    return json

    



