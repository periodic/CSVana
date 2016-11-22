function initFileReaderPorts(app) {
    app.ports.readFile.subscribe(function(fileInfo) {
        // readFileChunked(fileInfo.blob);
        var reader = new FileReader();
        reader.onload = function (e) {
            console.log("Got chunk: ", e);
            if (e.target.error == null) {
                app.ports.fileChunk.send(e.target.result);
            } else {
                console.error("Error reading file: " + e.target.error);
                return;
            }
        };

        reader.readAsText(fileInfo.blob);
    });

    function readFileChunked(file) {
        console.log("Reading file: ", file);
        var fileSize = file.size;
        var chunkSize = 1024 * 1024;
        var offset = 0;

        readChunk();

        function readChunk() {
            console.log("Reading chunk: ", offset);
            var reader = new FileReader();
            var blob = file.slice(offset, length + chunkSize);
            reader.onload = chunkReceived;
            reader.readAsText(blob);
            console.log(reader);
        }

        function chunkReceived(e) {
            console.log("Got chunk: ", e);
            if (e.target.error == null) {
                offset += e.target.result.length;
                app.ports.fileChunk.send(e.target.result);
            } else {
                console.error("Error reading file: " + e.target.error);
                return;
            }

            if (offset < fileSize) {
                readChunk();
            }
        }
    }
}
