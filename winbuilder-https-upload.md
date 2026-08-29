# Submitting to win-builder when FTP egress is blocked

## Why this exists

`devtools::check_win_release()` / `check_win_oldrelease()` /
`check_win_devel()` upload the package tarball to win-builder over
**FTP** (`ftp://win-builder.r-project.org/`). Some sandboxed
environments (e.g. the Claude Code dev container) block outbound FTP
while still allowing HTTPS. Symptom:

    Error in curl::curl_fetch_memory(url, handle = h) :
      Failure when receiving data from the peer [win-builder.r-project.org]:
      Recv failure: Connection reset by peer

Quick confirmation of the block:

``` bash
curl -sS -o /dev/null -w "FTP: %{http_code}\n" --max-time 20 \
  ftp://win-builder.r-project.org/      # -> curl (56) Connection reset
curl -sS -o /dev/null -w "HTTPS: %{http_code}\n" --max-time 20 \
  https://win-builder.r-project.org/    # -> HTTPS: 200
```

win-builder also offers an **official HTTPS web form** at
<https://win-builder.r-project.org/upload.aspx>. Using it via `curl` is
functionally identical to a human clicking “Upload File” in a browser —
it is *not* a backdoor. This doc records the working recipe.

## Prerequisites

1.  The standard FTP tool genuinely doesn’t work in this environment
    (verify with the curl test above). If FTP works, just use
    `devtools::check_win_*()` — don’t use this.

2.  Explicit user authorization to submit to win-builder (it’s an
    external upload that emails results to the maintainer).

3.  A built source tarball:

    ``` r

    devtools::build(args = "--compact-vignettes=both", path = "/tmp")
    # -> /tmp/<Pkg>_<Version>.tar.gz
    ```

## The form (as of 2026-05)

`upload.aspx` is an ASP.NET WebForms page. One `<form>` with
`enctype="multipart/form-data"`, posting to `./upload.aspx`. Hidden
state fields plus four file inputs:

| Field         | Submit button | Target R version           |
|---------------|---------------|----------------------------|
| `FileUpload1` | `Button1`     | **R-release**              |
| `FileUpload2` | `Button2`     | **R-devel**                |
| `FileUpload4` | `Button4`     | R-devel UCRT (usually off) |
| `FileUpload3` | `Button3`     | **R-oldrelease**           |

Required hidden fields (values change per page load — scrape them from a
fresh GET): `__VIEWSTATE`, `__VIEWSTATEGENERATOR`, `__EVENTVALIDATION`.

Each POST submits exactly one file field + its button + the three hidden
fields. Do a **fresh GET before each POST** (one tarball at a time) and
carry cookies in a jar. Force **HTTP/1.1** — IIS + curl’s HTTP/2
multipart upload intermittently fails with
`HTTP/2 stream 1 was not closed cleanly: CANCEL (err 8)`.

## Recipe

``` bash
#!/bin/bash
set -e
TARBALL="/tmp/Require_2.0.0.tar.gz"   # <- edit
JAR=$(mktemp)

submit() {
  local fileField="$1" btnField="$2" label="$3" page vs vsg ev code
  page=$(curl -sS --http1.1 -c "$JAR" -b "$JAR" \
         https://win-builder.r-project.org/upload.aspx)
  vs=$(printf '%s' "$page"  | grep -o 'id="__VIEWSTATE" value="[^"]*"'          | sed 's/.*value="//;s/"$//')
  vsg=$(printf '%s' "$page" | grep -o 'id="__VIEWSTATEGENERATOR" value="[^"]*"' | sed 's/.*value="//;s/"$//')
  ev=$(printf '%s' "$page"  | grep -o 'id="__EVENTVALIDATION" value="[^"]*"'    | sed 's/.*value="//;s/"$//')
  code=$(curl -sS --http1.1 -c "$JAR" -b "$JAR" \
    -o "/tmp/wb_resp_${label}.html" -w "%{http_code}" \
    -F "__VIEWSTATE=${vs}" \
    -F "__VIEWSTATEGENERATOR=${vsg}" \
    -F "__EVENTVALIDATION=${ev}" \
    -F "${fileField}=@${TARBALL};type=application/gzip" \
    -F "${btnField}=Upload File" \
    https://win-builder.r-project.org/upload.aspx)
  echo "${label}: HTTP ${code}"
}

submit FileUpload1 Button1 R-release
submit FileUpload2 Button2 R-devel
submit FileUpload3 Button3 R-oldrelease
rm -f "$JAR"
```

## Verifying acceptance (HTTP 200 is NOT enough)

A 200 just means the page rendered. win-builder confirms receipt by
echoing the file into a `Label<N>` span in the *response to that POST*
(`Label1` for release, `Label2` for devel, `Label3` for oldrelease):

``` bash
for v in R-release R-devel R-oldrelease; do
  echo "=== $v ==="
  grep -oE '<span id="Label[0-9]"[^>]*>[^<]*File name[^<]*' \
    "/tmp/wb_resp_${v}.html"
done
# Each must print:  File name: <Pkg>_<Version>.tar.gz
```

If the response is just the empty form (~2.9 KB, no populated
`Label<N>`), the upload did NOT register — recheck the hidden-field
scraping and that you used HTTP/1.1.

Results are emailed to the package’s `Maintainer` address (from
`DESCRIPTION`), typically within 30–60 min, with per-build log URLs at
`https://win-builder.r-project.org/<token>/`.

## Notes / gotchas

- The `FileUpload1..4` ↔︎ R-version mapping is by document order on the
  page; the UCRT form (`FileUpload4`) is often HTML-commented-out but
  the field still exists. Re-scrape if win-builder changes the layout.
- Don’t try to batch all three in one POST — ASP.NET processes one
  button per round-trip.
- Clean up `/tmp/wb_resp_*.html` and any temp script afterward.
- This is a *fallback*. Prefer `devtools::check_win_*()` whenever FTP
  egress is available.
