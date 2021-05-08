# Building Web Demo with Asterius

Building the web demo with Asterius is a bit finicky:

1. Copy math-parse source files into `app/`, so that `ahc-link` can find them.
   This is because `--export-function` is broken for `ahc-dist`, so we can't
   use Cabal to discover deps.

2. Load and run the Asterius docker image, mounting this repo inside the container - I use `/workspace`.

   ```sh
   $ sudo docker run -it --rm -v $(pwd):/workspace terrorjack/asterius:latest
   root@bbfcf2015f28:~#
   ```

3. From within the container, navigate to the mounted repo (in my case
   `/workspace`) and run `ahc-link` with the following flags:

   ```sh
   root@bbfcf2015f28:~# cd /workspace
   root@bbfcf2015f28:~# ahc-link \
        --input-hs app/Asterius.hs \
        --no-main --export-function tryStrToInt --export-function tryStrToPronounciation \
        --browser --bundle --input-mjs app/custom-entry.mjs
   ```

4. Exit the docker container, and copy the relevant generated files to `docs/`:
   ```sh
   $ cp app/Asterius.js app/Asterius.wasm docs/
   ```

Now serve `docs/`, either with Github Pages or your own host. Enjoy!
