import argparse
import io
import numpy as np
import platypus
import requests
import time

def get_array(url):
    r = requests.get(url)
    r.raise_for_status()
    return np.load(io.BytesIO(r.content))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="platypus_client.py",
                                     description="Interpret Platypus games for a server")
    parser.add_argument("host")
    parser.add_argument("name")
    args = parser.parse_args()
    host, name = args.host, args.name
    print("Retrieving players...")
    players = get_array(host + "/machines")
    print("Retrieving weights...")
    weights = get_array(host + "/weights")
    platypus.load(players, weights)

    while True:
        fetch_start = time.time()
        r = requests.get(host + "/work")
        fetch_time = time.time() - fetch_start
        if r.status_code == 200:
            job = r.json()
            results = []
            compute_start = time.time()
            for i in range(job["index"] * job["size"],
                           min((1 + job["index"]) * job["size"], len(players))):
                wins, points = platypus.run(players[i])
                results.append({"id": i, "wins": wins, "points": points})
            telemetry = {"compute": time.time() - compute_start,
                         "fetch": fetch_time}
            requests.post(host + "/store", json={"results": results, "telemetry": telemetry, "name": name})
        elif r.status_code == 404:
            print("Server says we're done.")
            break
        else:
            r.raise_for_status()
