package io.github.mmhelloworld.idrisjvm;

import IdrisJvm.IR.export.ExportIFace;
import IdrisJvm.IR.export.ListExportIFace;
import IdrisJvm.IR.export.ListSDecl;
import IdrisJvm.IR.export.SDecl;
import idris.prelude.list.ListExport;
import idris.prelude.list.ListFDesc;
import idris.prelude.list.ListLVar;
import idris.prelude.list.ListSAlt;
import idris.prelude.list.ListString;
import IdrisJvm.IR.export.Export;
import IdrisJvm.IR.export.FDesc;
import IdrisJvm.Core.export.Codegen;
import IdrisJvm.IR.export.LVar;
import IdrisJvm.IR.export.MaybeLVar;
import IdrisJvm.IR.export.SAlt;
import IdrisJvm.IR.export.SForeignArg;
import IdrisJvm.IR.export.SForeignArgs;

import java.util.List;
import java.util.ListIterator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class Converters {

    public static ListFDesc toIdrisListFDesc(List<FDesc> jlist) {
        return toIdrisList(jlist, Codegen::emptyFDesc, Codegen::consFDesc);
    }

    public static ListExport toIdrisListExport(List<Export> jlist) {
        return toIdrisList(jlist, Codegen::emptyExport, Codegen::consExport);
    }

    public static ListString toIdrisListString(List<String> jlist) {
        return toIdrisList(jlist, Codegen::emptyListString, Codegen::consString);
    }

    public static ListLVar toIdrisListLVar(List<LVar> jlist) {
        return toIdrisList(jlist, Codegen::emptyLVar, Codegen::consLVar);
    }

    public static ListSAlt toIdrisListSAlt(List<SAlt> jlist) {
        return toIdrisList(jlist, Codegen::emptySAlt, Codegen::consSAlt);
    }

    public static ListSDecl toIdrisListSDecl(List<SDecl> jlist) {
        return toIdrisList(jlist, Codegen::emptySDecl, Codegen::consSDecl);
    }

    public static ListExportIFace toIdrisListExportIFace(List<ExportIFace> jlist) {
        return toIdrisList(jlist, Codegen::emptyExportIFace, Codegen::consExportIFace);
    }

    public static SForeignArgs toIdrisListSForeignArgs(List<SForeignArg> jlist) {
        return toIdrisList(jlist, Codegen::emptySForeignArg, Codegen::consSForeignArg);
    }

    public static MaybeLVar nullableToMaybeLVar(LVar nullableLVar) {
        return nullableToMaybe(nullableLVar, Codegen::justLVar, Codegen::nothingLVar);
    }

    private static <JType, IdrisType> IdrisType nullableToMaybe(JType nullableValue,
                                                               Function<JType, IdrisType> justSupplier,
                                                               Supplier<IdrisType> nothingSupplier) {
        return Optional.ofNullable(nullableValue)
            .map(justSupplier)
            .orElseGet(nothingSupplier);
    }

    private static <E, T> T toIdrisList(List<E> xs, Supplier<T> emptySupplier, BiFunction<E, T, T> cons) {
        final ListIterator<E> reverseItr = xs.listIterator(xs.size());
        T res = emptySupplier.get();
        while (reverseItr.hasPrevious()) {
            res = cons.apply(reverseItr.previous(), res);
        }
        return res;
    }
}
