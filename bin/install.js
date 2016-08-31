var ZipInputStream = Java.type('java.util.zip.ZipInputStream');
var FileInputStream = Java.type('java.io.FileInputStream');
var Paths = Java.type('java.nio.file.Paths');
var StandardCopyOption = Java.type('java.nio.file.StandardCopyOption');
var File = Java.type('java.io.File');
var URL = Java.type('java.net.URL');
var System = Java.type('java.lang.System');
var Files = Java.type('java.nio.file.Files');

var dfltWorkingDir = System.getProperty('user.home') + File.separator + ".idrisjvm";
var workingDir = System.getProperty('IDRIS_JVM_WORK_DIR', dfltWorkingDir);

function downloadUrl(url, outFilePath) {
  url = new URL(url);

  if (!outFilePath) {
    var path = Paths.get(url.getPath());
    outFilePath = path.getName(path.getNameCount() - 1);
  }

  var connection = url.openConnection();
  var input = connection.getInputStream();
  Files.copy(input,
    Paths.get(workingDir, outFilePath),
    StandardCopyOption.REPLACE_EXISTING);
}

function unzip(zipFilePath, destDirectory) {
  destDirectory = destDirectory || workingDir;
  var destDir = new File(destDirectory);
  destDir.mkdir();
  var zipIn = new ZipInputStream(new FileInputStream(zipFilePath))

  var entry = zipIn.getNextEntry();
  while (entry) {
      var filePath = destDirectory + File.separator + entry.getName();
      if (!entry.isDirectory()) {
          var destPath = Paths.get(filePath);
          destPath.getParent().toFile().mkdirs();
          Files.copy(zipIn,
            destPath,
            Java.to([StandardCopyOption.REPLACE_EXISTING], 'java.nio.file.CopyOption[]'));
      } else {
          var dir = new File(filePath);
          dir.mkdir();
      }
      zipIn.closeEntry();
      entry = zipIn.getNextEntry();
  }
}

function install() {
  var runtimeLib = 'https://github.com/mmhelloworld/idrisjvm-runtime/releases/download/1.0-SNAPSHOT/idrisjvm-runtime-1.0-SNAPSHOT.jar';
  var assemblerLib = 'https://github.com/mmhelloworld/jvm-assembler/releases/download/1.0-SNAPSHOT/jvm-assembler-server-1.0-SNAPSHOT.zip';

  downloadUrl(runtimeLib);
  downloadUrl(assemblerLib);
  unzip(workingDir + File.separator + 'jvm-assembler-server-1.0-SNAPSHOT.zip');
}

install();
