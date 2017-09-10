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
      const link = document.createElement("a");
      link.href = url;
      link.download = "document.pdf";
      document.body.appendChild(link);
      link.click();
    });
  // TODO handle errors
}
