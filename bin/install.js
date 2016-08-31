var JString = Java.type('java.lang.String');
var ZipInputStream = Java.type('java.util.zip.ZipInputStream');
var FileInputStream = Java.type('java.io.FileInputStream');
var Paths = Java.type('java.nio.file.Paths');
var StandardCopyOption = Java.type('java.nio.file.StandardCopyOption');
var File = Java.type('java.io.File');
var URL = Java.type('java.net.URL');
var System = Java.type('java.lang.System');
var Files = Java.type('java.nio.file.Files');

var RUNTIME_VERSION = '1.0-SNAPSHOT';
var ASSEMBLER_VERSION = '1.0-SNAPSHOT';

var dfltWorkingDir = System.getProperty('user.home') + File.separator + '.idrisjvm';
var workingDir = System.getenv('IDRIS_JVM_WORK_DIR') || dfltWorkingDir;

function downloadUrl(url, outFilePath) {
  url = new URL(url);

  if (!outFilePath) {
    var urlPath = Paths.get(url.getPath());
    var lastPart = urlPath.getName(urlPath.getNameCount() - 1).toString();
    outFilePath = Paths.get(workingDir, lastPart).toString();
  }

  var connection = url.openConnection();
  var input = connection.getInputStream();
  Files.copy(input,
    Paths.get(outFilePath),
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
            StandardCopyOption.REPLACE_EXISTING);
      } else {
          var dir = new File(filePath);
          dir.mkdir();
      }
      zipIn.closeEntry();
      entry = zipIn.getNextEntry();
  }
}

function install() {
  var assemblerZip = JString.format('jvm-assembler-server-%s.zip', ASSEMBLER_VERSION);
  var runtimeLib = JString.format('https://github.com/mmhelloworld/idrisjvm-runtime/releases/download/%1$s/idrisjvm-runtime-%1$s.jar', RUNTIME_VERSION);
  var assemblerLib = JString.format('https://github.com/mmhelloworld/jvm-assembler/releases/download/%s/%s', ASSEMBLER_VERSION, assemblerZip);

  new File(workingDir).mkdirs();
  downloadUrl(runtimeLib);
  downloadUrl(assemblerLib);
  unzip(workingDir + File.separator + assemblerZip);
}

install();
