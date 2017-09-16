package io.github.mmhelloworld.idrisjvm;

import IdrisJvm.IR.export.ArithTy;
import IdrisJvm.IR.export.Const;
import IdrisJvm.IR.export.Export;
import IdrisJvm.IR.export.ExportIFace;
import IdrisJvm.IR.export.FDesc;
import IdrisJvm.IR.export.LVar;
import IdrisJvm.IR.export.PrimFn;
import IdrisJvm.IR.export.SAlt;
import IdrisJvm.IR.export.SDecl;
import IdrisJvm.IR.export.SExp;
import io.github.mmhelloworld.idrisjvm.model.ArithTyDeserializer;
import io.github.mmhelloworld.idrisjvm.model.ConstDeserializer;
import io.github.mmhelloworld.idrisjvm.model.ExportDeserializer;
import io.github.mmhelloworld.idrisjvm.model.ExportIFaceDeserializer;
import io.github.mmhelloworld.idrisjvm.model.JFDescDeserializer;
import io.github.mmhelloworld.idrisjvm.model.LVarDeserializer;
import io.github.mmhelloworld.idrisjvm.model.PrimFnDeserializer;
import io.github.mmhelloworld.idrisjvm.model.SAltDeserializer;
import io.github.mmhelloworld.idrisjvm.model.SDeclDeserializer;
import io.github.mmhelloworld.idrisjvm.model.SExpDeserializer;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

@SpringBootApplication
public class IdrisJvmCodegenApplication {
    public static void main(String[] args) {
        SpringApplication.run(IdrisJvmCodegenApplication.class, args);
    }

    @Bean
    public Jackson2ObjectMapperBuilder objectMapperBuilder() {
        Jackson2ObjectMapperBuilder builder = new Jackson2ObjectMapperBuilder();
        builder.deserializerByType(FDesc.class, new JFDescDeserializer());
        builder.deserializerByType(LVar.class, new LVarDeserializer());
        builder.deserializerByType(ArithTy.class, new ArithTyDeserializer());
        builder.deserializerByType(PrimFn.class, new PrimFnDeserializer());
        builder.deserializerByType(Const.class, new ConstDeserializer());
        builder.deserializerByType(SAlt.class, new SAltDeserializer());
        builder.deserializerByType(SExp.class, new SExpDeserializer());
        builder.deserializerByType(SDecl.class, new SDeclDeserializer());

        builder.deserializerByType(ExportIFace.class, new ExportIFaceDeserializer());
        builder.deserializerByType(Export.class, new ExportDeserializer());
        return builder;
    }

}
