function generatePDF(file, pages) {
  const body = new FormData();
  body.append("pages", pages);
  body.append("file", file);
  return fetch("/api", {
    method: "POST",
    body: body
  })
    .then(response => response.blob())
    .then(blob => {
      const url = URL.createObjectURL(blob);
      window.location.href = url;
    });
  // TODO handle errors
}
